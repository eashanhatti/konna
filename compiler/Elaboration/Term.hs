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
import qualified Elaboration.Items as EI
import Control.Algebra(Has)
import qualified Control.Effect.Error as EE
import Data.Bifunctor
import Data.Map(toList, size, singleton)
import Data.Foldable(find)

check :: Elab sig m => TermAst -> N.Value -> m (C.Term, TermAst)
check term goal = do
  univ <- typeOf goal
  cGoal <- readback goal
  r <- case (term, N.unVal goal) of
    (FocusedAst side term, _) -> do
      (cTerm, term') <- check term goal
      pure (cTerm, FocusedAst side term')
    (ErrorAst _ term, _) -> check term goal
    (TermAst (Var name), _) -> do
      tys <- getVarSigs name
      case size tys of
        0 -> elabError term (UnboundVar name)
        _ -> do
          urs <- mapM (\p@(vty, _) -> unify goal vty >>= \errs -> pure (errs, p)) (toList tys)
          case find (null . fst) urs of
            Just (_, (vty, LocalVar ix)) -> do
              cVty <- readback vty
              pure (C.gen $ C.Var ix cVty, term)
            Just (_, (vty, GlobalVar nid)) -> do
              cVty <- readback vty
              pure (C.gen $ C.GVar nid cVty, term)
            Nothing -> elabError term (MismatchVarType $ map fst urs)
    (TermAst (Lam names body), N.FunType _ _) -> do
      (inTys, outTy) <- funType goal
      let
        go :: Elab sig m => [NameAst] -> [(N.Value, N.Value)] -> m (C.Term, TermAst)
        go = curry \case
          (NameAst n:ns, (ty, lamTy):tys) -> do
            cLamTy <- readback lamTy
            (cLam, bodyAst) <- bind n ty (go ns tys)
            pure (C.gen $ C.FunIntro cLam cLamTy, bodyAst)
          (n:ns, []) -> do
            putError $ ParamNum (length inTys) (length names)
            failElab
          ([], _) -> check body outTy
      go names inTys `onFail` pure (C.gen C.ElabError, term)
    (TermAst (Ann term ty), _) -> do
      (cTy, ty') <- check ty univ
      vTy <- eval cTy
      unify goal vTy >>= mapM (putError . UnifyError)
      check term vTy
    (TermAst (Pi (NameAst name) inTy outTy), N.TypeType1) -> do
      (cInTy, inTy') <- check inTy (N.gen N.TypeType1)
      vInTy <- eval cInTy
      (cOutTy, outTy') <- bind name vInTy $ check outTy (N.gen N.TypeType1)
      pure (C.gen $ C.FunType cInTy cOutTy, TermAst $ Pi (NameAst name) inTy' outTy')
    (TermAst (Arrow inTy outTy), N.TypeType0) -> do
      (cInTy, inTy') <- check inTy (N.gen N.TypeType0)
      vInTy <- eval cInTy
      (cOutTy, outTy') <- bindUnnamed vInTy $ check outTy (N.gen N.TypeType0)
      pure (C.gen $ C.FunType cInTy cOutTy, TermAst $ Arrow inTy' outTy')
    (TermAst U0, N.TypeType1) -> pure (C.gen C.TypeType0, term)
    (TermAst U1, N.TypeType1) -> pure (C.gen C.TypeType1, term)
    (TermAst (Code ty), N.TypeType1) -> do
      (cTy, ty') <- check ty (N.gen N.TypeType1)
      pure (C.gen $ C.QuoteType cTy, TermAst $ Code ty')
    (TermAst (Quote term), N.QuoteType vTy) -> do
      (cTerm, term') <- check term vTy
      pure (C.gen $ C.QuoteIntro cTerm cGoal, TermAst $ Quote term')
    (TermAst (Splice term), _) -> do
      (cTerm, term') <- check term (N.gen $ N.QuoteType goal)
      pure (C.gen $ C.QuoteElim cTerm, TermAst $ Splice term')
    (TermAst Hole, _) -> do
      meta <- freshMeta goal >>= readback
      pure (meta, term)
    (TermAst (Let bindings body), _) -> do
      (cBindings, bindings') <- unzip <$> EI.check bindings
      (cBody, body') <- go (zip cBindings (map unName bindings'))
      pure (C.gen $ C.Letrec cBindings cBody, TermAst $ Let bindings' body')
      where
        go :: Elab sig m => [(C.Item, Name)] -> m (C.Term, TermAst)
        go = \case
          [] -> check body goal
          (C.TermDef _ sig def, name):bs -> bindGlobal name sig $ defineGlobal name (DTermDef def) $ const $ go bs
          (C.IndDef _ sig, name):bs -> bindGlobal name sig $ defineGlobal name DIndDef $ const $ go bs
          (C.ConDef _ sig, name):bs -> bindGlobal name sig $ defineGlobal name DConDef $ const $ go bs
    (_, _) -> do
      (cTerm, ty, term') <- infer term
      unify goal ty >>= mapM (putError . UnifyError)
      pure (cTerm, term')
  errors <- getErrors
  pure $ (second $ ErrorAst errors) r

infer :: Elab sig m => TermAst -> m (C.Term, N.Value, TermAst)
infer term = do
  (cTerm, vTy, term') <- case term of
    FocusedAst side term -> do
      (cTerm, ty, term') <- infer term
      pure (cTerm, ty, FocusedAst side term')
    ErrorAst _ term -> infer term
    TermAst (Var name) -> do
      tys <- getVarSigs name
      case toList tys of
        [] -> elabErrorTy term (UnboundVar name)
        tys@[(ty, LocalVar ix)] -> do
          cTy <- readback ty
          pure (C.gen $ C.Var ix cTy, ty, term)
        tys@[(ty, GlobalVar nid)] -> do
          cTy <- readback ty
          pure (C.gen $ C.GVar nid cTy, ty, term)
        tys -> elabErrorTy term (AmbiguousVar (map fst tys))
    TermAst (App lam args) -> do
      (cLam, lamTy, lam') <- infer lam
      (inTys, outTy) <- funType lamTy
      if length inTys == length args then do
        cArgs <- mapM (uncurry check) (zip args (map fst inTys))
        let cApp = foldl (\l a -> C.gen $ C.FunElim l a) cLam (map fst cArgs)
        pure (cApp, outTy, TermAst $ App lam' (map snd cArgs))
      else
        elabErrorTy term (ArgNum (length inTys) (length args))
  errors <- getErrors
  pure (cTerm, vTy, ErrorAst errors term')

funType :: Elab sig m => N.Value -> m ([(N.Value, N.Value)], N.Value)
funType val = case N.unVal val of
  N.FunType inTy outTy -> do
    outTy <- closureToValue outTy inTy
    (inTys, finalOutTy) <- funType outTy
    pure ((inTy, val):inTys, finalOutTy)
  _ -> pure ([], val)