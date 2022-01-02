{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elaboration.Term where

import Surface as S
import qualified Core as C
import qualified Norm as N
import qualified Unification as U
import Var
import Elaboration.Effect
import Elaboration.Error
import Control.Algebra(Has)
import qualified Control.Effect.Error as EE
import Data.Bifunctor
import Data.Map(toList)
import Data.Foldable(find)

check :: Elab sig m => TermAst -> N.Value -> m (C.Term, TermAst)
check term goal = do
  r <- case (term, goal) of
    (FocusedAst side term, _) -> do
      (cTerm, term') <- check term goal
      pure (cTerm, FocusedAst side term')
    (ErrorAst _ term, _) -> check term goal
    (TermAst (Var name), _) -> do
      tys <- getVarTypes name
      case tys of
        Just tys -> do
          urs <- mapM (\p@(vty, _) -> unify goal vty >>= \errs -> pure (errs, p)) (toList tys)
          case find (null . fst) urs of
            Just (_, (vty, ix)) -> do
              cVty <- readback vty
              pure (C.gen $ C.Var ix cVty, term)
            Nothing -> elabError term (MismatchVarType $ map fst urs)
        Nothing -> elabError term (UnboundVar name)
    (TermAst (Lam names body), N.Value (N.FunType _ _)) -> do
      (inTys, outTy) <- funType goal
      let
        go :: Elab sig m => [NameAst] -> [(N.Value, N.Value)] -> m (C.Term, TermAst)
        go ns tys = case (ns, tys) of
          (NameAst n:ns, (ty, lamTy):tys) -> do
            cLamTy <- readback lamTy
            (cLam, bodyAst) <- bind n ty (go ns tys)
            pure (C.gen $ C.FunIntro cLam cLamTy, bodyAst)
          (n:ns, []) -> do
            putError $ TooManyParams (length inTys) (length names)
            failElab
          ([], _) -> check body outTy
      go names inTys `onFail` pure (C.gen C.ElabError, term)
    (TermAst (Ann term ty), _) -> do
      (cTy, ty') <- checkType ty
      undefined
  errors <- getErrors
  pure $ (second $ ErrorAst errors) r

checkType :: Elab sig m => TermAst -> m (C.Term, TermAst)
checkType ty = getUniv >>= check ty

funType :: Elab sig m => N.Value -> m ([(N.Value, N.Value)], N.Value)
funType val = case N.unVal val of
  N.FunType inTy outTy -> do
    outTy <- closureToValue outTy inTy
    (inTys, finalOutTy) <- funType outTy
    pure ((inTy, val):inTys, finalOutTy)
  _ -> pure ([], val)