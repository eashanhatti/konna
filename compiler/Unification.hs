{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# OPTIONS_GHC -fdefer-type-errors #-}

module Unification where

import qualified Data.Map as Map
import qualified Norm as N
import qualified Core as C
import qualified Surface as S
import Var
import Data.List(uncons)
import Data.Maybe(fromJust)
import Data.Functor.Identity(Identity)
import Control.Monad.State(State, state, runState)
import Control.Monad.Reader(runReader)
import Control.Monad.Except(ExceptT, lift, throwError, liftEither, runExceptT)
import Control.Monad(ap, liftM, forM_)
import qualified Data.Set as Set
import GHC.Stack
import Debug.Trace
import Data.Data(Data)

data Error
  = InvalidSpine
  | OccursCheck
  | EscapingVar
  | Mismatch N.Value N.Value
  | MismatchSpines N.Spine N.Spine
  deriving Data

instance Show Error where
  show e = case e of
    InvalidSpine -> "Invalid Spine "
    OccursCheck -> "Failed Occurs Check"
    EscapingVar -> "Escaping Var"
    Mismatch val val' -> "Mismatched Types:\n  " ++ show val ++ "\n  " ++ show val'
    MismatchSpines s s' -> "Mismatched Spines\n  " ++ show s ++ "\n  " ++ show s'

newtype Unify a = Unify (State (N.Metas, [Error], N.Globals) a)
  deriving (Functor, Applicative, Monad) via (State (N.Metas, [Error], N.Globals))

get :: Unify (N.Metas, [Error], N.Globals)
get = Unify $ state $ \s -> (s, s)

put :: (N.Metas, [Error], N.Globals) -> Unify ()
put s = Unify $ state $ \_ -> ((), s)

runUnify :: Unify a -> (N.Metas, [Error], N.Globals) -> (a, (N.Metas, [Error], N.Globals))
runUnify (Unify act) s = runState act s

runNorm :: Level -> N.Norm a -> Unify a
runNorm lv act = do
  (metas, _, globals) <- get
  pure $ runReader act (lv, metas, [], globals)

putSolution :: Global -> N.Value -> Unify ()
putSolution gl sol = do
  (metas, errors, globals) <- get
  put (Map.insert gl (N.Solved sol) metas, errors, globals)

putError :: Error -> Unify ()
putError err = do
  (metas, errors, globals) <- get
  put (metas, err:errors, globals)

getMetas :: Unify N.Metas
getMetas = do
  (metas, _, _) <- get
  pure metas

data PartialRenaming = PartialRenaming
  { domain :: Level
  , codomain :: Level
  , ren :: Map.Map Level Level }

inc :: PartialRenaming -> PartialRenaming
inc pren = PartialRenaming
  (Level $ unLevel (domain pren) + 1)
  (Level $ unLevel (codomain pren) + 1)
  (Map.insert (codomain pren) (domain pren) (ren pren))

incN :: Int -> PartialRenaming -> PartialRenaming
incN n = case n of
  0 -> id
  n -> inc . (incN (n - 1))

invert :: N.Metas -> Level -> N.Spine -> ExceptT Error Unify PartialRenaming
invert metas gamma spine' = do
  let go :: N.Spine -> ExceptT Error Unify (Level, Map.Map Level Level)
      go = \case
        (arg:spine) -> do
          (domain, ren) <- go spine
          arg <- lift $ runNorm gamma $ N.force arg
          -- let !() = trace ("    Renaming = " ++ show ren ++ "  Arg = " ++ show arg ++ "  Spine = " ++ show spine') ()
          case N.unVal arg of
            N.StuckRigidVar _ lv [] | Map.notMember lv ren -> liftEither $ Right (Level $ unLevel domain + 1, Map.insert lv domain ren)
            _ -> throwError InvalidSpine
        [] -> liftEither $ Right (Level 0, mempty)
  (domain, ren) <- go spine'
  liftEither $ Right $ PartialRenaming domain gamma ren

rename :: HasCallStack => N.Metas -> Global -> PartialRenaming -> N.Value -> ExceptT Error Unify C.Term
rename metas gl pren rhs = go pren rhs
  where
    goSpine :: HasCallStack => PartialRenaming -> C.Term -> N.Spine -> ExceptT Error Unify C.Term
    goSpine pren val spine = case spine of
      (arg:spine) -> C.gen <$> (C.FunElim <$> goSpine pren val spine <*> go pren arg)
      [] -> liftEither $ Right val

    goMaybe :: HasCallStack => PartialRenaming -> Maybe (N.Value) -> ExceptT Error Unify (Maybe C.Term)
    goMaybe pren val = case val of
      Just val -> go pren val >>= pure . Just
      Nothing -> pure Nothing

    go :: HasCallStack => PartialRenaming -> N.Value -> ExceptT Error Unify C.Term
    go pren val = do
      val <- lift $ runNorm (domain pren) $ N.force val
      case N.unVal val of
        N.StuckFlexVar vty gl' spine ->
          if gl == gl'
          then throwError OccursCheck
          else do
            tty <- goMaybe pren vty
            goSpine pren (C.gen $ C.Meta gl' tty) spine
        N.StuckRigidVar vty lv spine -> case Map.lookup lv (ren pren) of
          Just lv' -> do
            tty <- go pren vty
            goSpine pren (C.gen $ C.Var (N.lvToIx (domain pren) lv') tty) spine
          Nothing -> throwError EscapingVar
        N.StuckSplice quote -> C.gen <$> (C.QuoteElim <$> go pren quote)
        N.FunIntro body vty@(N.unVal -> N.FunType inTy _) -> do
          tty <- go pren vty
          vBody <- lift $ runNorm (domain pren) $ N.appClosure body (N.gen $ N.StuckRigidVar inTy (codomain pren) [])
          bodyTerm <- go (inc pren) vBody
          liftEither $ Right $ C.gen $ C.FunIntro bodyTerm tty
        N.FunType inTy outTy -> do
          inTyTerm <- go pren inTy
          vOutTy <- lift $ runNorm (domain pren) $ N.appClosure outTy (N.gen $ N.StuckRigidVar inTy (codomain pren) [])
          outTyTerm <- go (inc pren) vOutTy
          liftEither $ Right $ C.gen $ C.FunType inTyTerm outTyTerm
        N.QuoteIntro inner ty -> do
          vInner <- lift $ runNorm (domain pren) $ N.eval inner
          innerTerm <- go pren vInner
          innerTy <- go pren ty
          liftEither $ Right $ C.gen $ C.QuoteIntro innerTerm innerTy
        N.QuoteType ty -> C.gen <$> (C.QuoteType <$> go pren ty)
        N.IndIntro nid cds ty -> C.gen <$> (C.IndIntro nid <$> mapM (go pren) cds <*> go pren ty)
        N.IndType nid indices -> mapM (go pren) indices >>= \is -> pure $ C.gen $ C.IndType nid is
        N.ProdType nid indices -> C.gen <$> (C.ProdType nid <$> mapM (go pren) indices)
        N.ProdIntro ty fields -> C.gen <$> (C.ProdIntro <$> go pren ty <*> mapM (go pren) fields)
        N.TypeType0 -> liftEither $ Right $ C.gen $ C.TypeType0
        N.TypeType1 -> liftEither $ Right $ C.gen $ C.TypeType1
        N.FunElim0 lam arg -> C.gen <$> (C.FunElim <$> go pren lam <*> go pren arg)
        N.Var0 ix ty -> C.gen <$> (C.Var <$> pure ix <*> go pren ty)
        -- N.Let0 def defTy body -> C.Let <$> go pren def <*> go pren defTy <*> go (inc pren) body
        N.Letrec0 defs body -> C.gen <$> (C.Letrec defs <$> go pren body)
        N.StuckGVar nid ty -> go pren ty >>= \ty -> pure $ C.gen $ C.GVar nid ty
        N.ElabError -> liftEither $ Right $ C.gen C.ElabError
        _ -> error $ show val

getTtySpine :: N.Metas -> Level -> N.Type -> N.Spine -> C.Term
getTtySpine metas lv vty spine = case (N.unVal vty, spine) of
  (N.FunType inTy outTy, _:spine) -> getTtySpine
    metas
    (incLevel lv)
    (runReader (N.appClosure outTy (N.gen $ N.StuckRigidVar inTy lv [])) (lv, metas, [], mempty))
    spine
  (_, []) -> runReader (N.readback vty) (lv, metas, [], mempty)

getTty :: N.Metas -> Level -> N.Value -> C.Term
getTty metas lv val = case N.unVal val of
  N.StuckFlexVar (Just vty) _ spine -> getTtySpine metas lv vty spine
  N.StuckFlexVar Nothing gl spine -> C.gen C.TypeType1 -- runReader (N.readback val) (lv, metas, [], mempty)
  N.StuckRigidVar vty _ spine -> getTtySpine metas lv vty spine
  N.StuckSplice _ -> C.gen C.TypeType1
  N.FunIntro _ vty -> runReader (N.readback vty) (lv, metas, [], mempty)
  N.FunType inTy _ -> getTty metas lv inTy
  N.QuoteType _ -> C.gen C.TypeType1
  N.QuoteIntro _ _ -> C.gen C.TypeType1
  N.TypeType0 -> C.gen C.TypeType1
  N.TypeType1 -> C.gen C.TypeType1
  N.FunElim0 _ _ -> C.gen C.TypeType0
  N.Var0 _ _ -> C.gen C.TypeType0
  N.IndIntro _ _ ty -> runReader (N.readback ty) (lv, metas, [], mempty)
  N.IndType _ _ -> C.gen C.TypeType1
  -- N.Let0 _ _ _ -> C.TypeType0
  N.Letrec0 _ _ -> C.gen C.TypeType0
  N.ElabError -> C.gen C.ElabError

getVtySpine :: N.Metas -> Level -> N.Type -> N.Spine -> N.Value
getVtySpine metas lv vty spine = case (N.unVal vty, spine) of
  (N.FunType inTy outTy, _:spine) -> getVtySpine
    metas
    (incLevel lv)
    (runReader (N.appClosure outTy (N.gen $ N.StuckRigidVar inTy lv [])) (lv, metas, [], mempty))
    spine
  (_, []) -> vty

getVty :: N.Metas -> Level -> N.Value -> N.Value
getVty metas lv val = case N.unVal val of
  N.StuckFlexVar (Just vty) _ spine -> getVtySpine metas lv vty spine
  N.StuckFlexVar Nothing _ spine -> N.gen N.TypeType1
  N.StuckRigidVar vty _ spine -> getVtySpine metas lv vty spine
  N.StuckSplice _ -> N.gen N.TypeType1
  N.FunIntro _ vty -> vty
  N.FunType inTy _ -> getVty metas lv inTy
  N.QuoteType _ -> N.gen N.TypeType1
  N.QuoteIntro _ _ -> N.gen N.TypeType1
  N.TypeType0 -> N.gen N.TypeType1
  N.TypeType1 -> N.gen N.TypeType1
  N.IndType _ _ -> N.gen N.TypeType1
  N.IndIntro _ _ ty -> ty
  N.FunElim0 _ _ -> N.gen N.TypeType0
  N.Var0 _ _ -> N.gen N.TypeType0
  -- N.Let0 _ _ _ -> N.TypeType0
  N.Letrec0 _ _ -> N.gen N.TypeType0
  N.ElabError -> N.gen N.ElabError

lams :: Level -> [C.Term] -> C.Term -> C.Term
lams lv ttys term = go (Level 0) ttys
  where
    go lv' ttys =
        if lv == lv'
        then term
        else
          let (tty, ttys) = fromJust $ uncons ttys
          in C.gen $ C.FunIntro (go (Level $ unLevel lv' + 1) ttys) tty

solve :: HasCallStack => Level -> Global -> N.Spine -> N.Value -> Unify ()
solve gamma gl spine rhs = do
  metas <- getMetas
  pren <- runExceptT $ invert metas gamma spine
  case pren of
    Right pren -> do
      rhs <- runExceptT $ rename metas gl pren rhs
      case rhs of
        Right rhs -> do
          solution <- runNorm gamma $ N.eval (lams (domain pren) (map (getTty metas (domain pren)) spine) rhs)
          putSolution gl solution
        Left err -> putError err
    Left err -> putError err

unifySpines :: HasCallStack => Level -> N.Spine -> N.Spine -> Unify ()
unifySpines lv spine spine'= case (spine, spine') of
  (arg:spine, arg':spine') -> do
    unifySpines lv spine spine'
    unify lv arg arg'
  ([], []) -> pure ()
  _ -> putError $ MismatchSpines spine spine'

-- level, goal, given
unify :: HasCallStack => Level -> N.Value -> N.Value -> Unify ()
unify lv val val' = do
  val <- runNorm lv $ N.force val
  val' <- runNorm lv $ N.force val'
  metasForGetVty <- getMetas
  case (N.unVal val, N.unVal val') of
    (N.FunIntro body vty@(N.unVal -> N.FunType inTy _), N.FunIntro body' vty'@(N.unVal -> N.FunType inTy' _)) -> do
      unify lv vty vty'
      vBody <- runNorm (incLevel lv) $ N.appClosure body (N.gen $ N.StuckRigidVar inTy lv [])
      vBody' <- runNorm (incLevel lv) $ N.appClosure body' (N.gen $ N.StuckRigidVar inTy' lv [])
      unify (incLevel lv) vBody vBody'
    (_, N.FunIntro body vty@(N.unVal -> N.FunType inTy' _)) | valTy@(N.unVal -> N.FunType inTy _) <- getVty metasForGetVty lv val -> do
      unify lv valTy vty
      vAppVal <- runNorm lv $ N.vApp val (N.gen $ N.StuckRigidVar inTy lv [])
      vBody <- runNorm (incLevel lv) $ N.appClosure body (N.gen $ N.StuckRigidVar inTy' lv [])
      unify (incLevel lv) vAppVal vBody
    (N.FunIntro body vty@(N.unVal -> N.FunType inTy _), _) | valTy@(N.unVal -> N.FunType inTy' _) <- getVty metasForGetVty lv val' -> do
      unify lv valTy vty
      vBody <- runNorm (incLevel lv) $ N.appClosure body (N.gen $ N.StuckRigidVar inTy lv [])
      vAppVal <- runNorm lv $ N.vApp val (N.gen $ N.StuckRigidVar inTy' lv [])
      unify (incLevel lv) vBody vAppVal
    (N.TypeType0, N.TypeType0) -> pure ()
    -- (N.TypeType1, N.TypeType0) -> pure ()
    (N.TypeType1, N.TypeType1) -> pure ()
    (N.FunType inTy outTy, N.FunType inTy' outTy') -> do
      unify lv inTy inTy'
      vOutTy <- runNorm (incLevel lv) $ N.appClosure outTy (N.gen $ N.StuckRigidVar inTy lv [])
      vOutTy' <- runNorm (incLevel lv) $ N.appClosure outTy' (N.gen $ N.StuckRigidVar inTy' lv [])
      unify (incLevel lv) vOutTy vOutTy'
    (N.QuoteType ty, N.QuoteType ty') -> unify lv ty ty'
    (N.QuoteIntro inner ty, N.QuoteIntro inner' ty') -> do
      vInner <- runNorm lv $ N.eval inner
      vInner' <- runNorm lv $ N.eval inner'
      unify lv vInner vInner'
      unify lv ty ty'
    (N.IndType nid indices, N.IndType nid' indices') | nid == nid' ->
      mapM_ (uncurry $ unify lv) (zip indices indices')
    (N.IndIntro nid cds ty, N.IndIntro nid' cds' ty') | nid == nid' && length cds == length cds' -> do
      mapM (uncurry $ unify lv) (zip cds cds')
      unify lv ty ty'
    (N.ProdType nid indices, N.ProdType nid' indices') | nid == nid' ->
      mapM_ (uncurry $ unify lv) (zip indices indices')
    (N.ProdIntro ty fields, N.ProdIntro ty' fields') -> do
      unify lv ty ty'
      mapM_ (uncurry $ unify lv) (zip fields fields')
    (N.StuckRigidVar vty rlv spine, N.StuckRigidVar vty' rlv' spine') | rlv == rlv' -> do
      unify lv vty vty'
      unifySpines lv spine spine'
    (N.StuckFlexVar (Just vty) gl spine, N.StuckFlexVar (Just vty') gl' spine') | gl == gl' -> do
      unify lv vty vty'
      unifySpines lv spine spine
    (N.StuckGVar nid _, N.StuckGVar nid' _) | nid == nid' -> pure ()
    (N.StuckFlexVar Nothing gl spine, N.StuckFlexVar Nothing gl' spine') | gl == gl' -> unifySpines lv spine spine
    -- FIXME? Unify types
    (_, N.StuckFlexVar _ gl spine) -> solve lv gl spine val
    (N.StuckFlexVar _ gl spine, _) -> solve lv gl spine val'
    (N.StuckSplice quote, N.StuckSplice quote') -> unify lv quote quote'
    (N.FunElim0 lam arg, N.FunElim0 lam' arg') -> do
      unify lv lam arg
      unify lv arg arg'
    (N.Var0 ix ty, N.Var0 ix' ty') | ix == ix' -> unify lv ty ty'
    -- (N.Let0 def defTy body, N.Let0 def' defTy' body') -> do
    --   unify lv def def'
    --   unify lv defTy defTy'
    --   unify (incLevel lv) body body'
    (N.Letrec0 defs body, N.Letrec0 defs' body') -> putError $ Mismatch val val' -- TODO
    (N.ElabError, _) -> pure ()
    (_, N.ElabError) -> pure ()
    _ -> putError $ Mismatch val val'