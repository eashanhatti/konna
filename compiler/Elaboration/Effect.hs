{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Elaboration.Effect where

import Control.Algebra(Has, run, Algebra)
import qualified Control.Effect.State as SE
import qualified Control.Effect.Reader as RE
import qualified Control.Effect.Error as EE
import Elaboration.Error
import qualified Control.Monad.Reader as R
import Control.Monad.Identity(Identity)
import qualified Norm as N
import qualified Surface as S
import qualified Unification as U
import qualified Core as C
import Data.Map(Map, lookup, insert, singleton, (!))
import Data.Set(Set)
import Data.Maybe(fromMaybe)
import Var hiding(unLevel)
import Numeric.Natural
import Control.Carrier.Error.Either(runError, ErrorC)
import Control.Carrier.State.Strict(runState, StateC)
import Control.Carrier.Reader(runReader, ReaderC)
import Prelude hiding (lookup)

data State = State
  { unErrors :: [Error]
  , unMetas :: N.Metas
  , nextMeta :: Int
  , nextId :: Int }
  deriving Show

data VarEntry = LocalVar Index | GlobalVar Id

data Context = Context
  { unLocals :: N.Locals
  , unGlobals :: N.Globals
  , unVarSigs :: Map S.Name (Map N.Value VarEntry)
  , unLevel :: Level
  , unBinderInfo :: [C.BinderInfo]
  , unNamesToIds :: Map S.Name Id }

type Elab sig m = (Has (RE.Reader Context) sig m, Has (SE.State State) sig m, Has (EE.Error ()) sig m)

runElab :: ErrorC () (ReaderC Context (StateC State Identity)) a -> (State, Either () a)
runElab act =
  run .
  runState (State mempty mempty 0 0) .
  runReader (Context mempty mempty mempty (Level 0) [] mempty) .
  (runError{- :: ErrorC () _ _ -> _ (Either () a)-}) $
  act

getErrors :: Elab sig m => m [Error]
getErrors = do
  state <- SE.get
  SE.put $ state { unErrors = [] }
  pure $ unErrors state

getVarSigs :: Elab sig m => S.Name -> m (Map N.Value VarEntry)
getVarSigs name = lookup name . unVarSigs <$> RE.ask >>= \case
  Just sigs -> pure sigs
  Nothing -> pure mempty

getVar :: Elab sig m => S.Name -> N.Value -> m VarEntry
getVar name sig = lookup name . unVarSigs <$> RE.ask >>= \case
  Just (lookup sig -> Just entry) -> pure entry

unify :: Elab sig m => N.Value -> N.Value -> m [U.Error]
unify val val' = do
  state <- SE.get
  context <- RE.ask
  let ((), (newMetas, errors, _)) = U.runUnify (U.unify (unLevel context) val val') (unMetas state, [], unGlobals context)
  pure errors

putError :: Elab sig m => Error -> m ()
putError err = do
  state <- SE.get
  SE.put $ state { unErrors = err:(unErrors state) }

elabError :: Elab sig m => S.Ast a -> Error -> m (C.Term, S.Ast a)
elabError ast err = do
  putError err
  pure (C.gen C.ElabError, ast)

elabErrorTy :: Elab sig m => S.Ast a -> Error -> m (C.Term, N.Value, S.Ast a)
elabErrorTy ast err = do
  putError err
  pure (C.gen C.ElabError, N.gen N.ElabError, ast)

closureToValue :: Elab sig m => N.Closure -> N.Value -> m N.Value
closureToValue closure sig = do
  state <- SE.get
  context <- RE.ask
  pure $ R.runReader (N.appClosure closure (N.gen $ N.StuckRigidVar sig (unLevel context) [])) (unLevel context, unMetas state, unLocals context, unGlobals context)

bind :: Elab sig m => S.Name -> N.Value -> m a -> m a
bind name sig act = do
  context <- RE.ask
  let entry = fromMaybe mempty (lookup name (unVarSigs context))
  RE.local
    (const $ context
      { unLocals = (N.gen $ N.StuckRigidVar sig (unLevel context) []):(unLocals context)
      , unLevel = incLevel (unLevel context)
      , unVarSigs = insert name (insert sig (LocalVar $ Index 0) entry) (unVarSigs context)
      , unBinderInfo = C.Abstract:(unBinderInfo context) })
    act

bindGlobal :: Elab sig m => S.Name -> C.Term -> m a -> m a
bindGlobal name sig act = do
  context <- RE.ask
  vSig <- eval sig
  let entry = fromMaybe mempty (lookup name (unVarSigs context))
  nid <- freshId
  RE.local
    (const $ context
      { unVarSigs = insert name (insert vSig (GlobalVar nid) entry) (unVarSigs context)
      , unGlobals = insert nid (C.SigDef nid sig) (unGlobals context)
      , unNamesToIds = insert name nid (unNamesToIds context) })
    act

data ItemDef
  = DTermDef C.Term
  | DIndDef
  | DProdDef
  | DConDef

defineGlobal :: Elab sig m => S.Name -> ItemDef -> (C.Item -> m a) -> m a
defineGlobal name def act = do
  context <- RE.ask
  let nid = unNamesToIds context ! name
  let C.SigDef _ sig = unGlobals context ! nid
  let
    item = case def of
      DTermDef def -> C.TermDef nid sig def
      DIndDef -> C.IndDef nid sig
      DProdDef -> C.ProdDef nid sig
      DConDef -> C.ConDef nid sig
  RE.local
    (const $ context
      { unGlobals = insert nid item (unGlobals context) })
    (act item)

bindUnnamed :: Elab sig m => N.Value -> m a -> m a
bindUnnamed sig act = do
  context <- RE.ask
  RE.local
    (const $ context
      { unLocals = (N.gen $ N.StuckRigidVar sig (unLevel context) []):(unLocals context)
      , unLevel = incLevel (unLevel context)
      , unBinderInfo = C.Abstract:(unBinderInfo context) })
    act

freshId :: Elab sig m => m Id
freshId = do
  state <- SE.get
  SE.put $ state { nextId = nextId state + 1 }
  pure $ Id (nextId state)

failElab :: Elab sig m => m a
failElab = EE.throwError ()

onFail :: Elab sig m => m a -> m a -> m a
onFail act e = act `EE.catchError` (\() -> e)

readback :: Elab sig m => N.Value -> m C.Term
readback val = do
  state <- SE.get
  context <- RE.ask
  pure $ R.runReader (N.readback val) (unLevel context, unMetas state, unLocals context, unGlobals context)

eval :: Elab sig m => C.Term -> m N.Value
eval term = do
  state <- SE.get
  context <- RE.ask
  pure $ R.runReader (N.eval term) (unLevel context, unMetas state, unLocals context, unGlobals context)

typeOf :: Elab sig m => N.Value -> m N.Value
typeOf val = do
  state <- SE.get
  context <- RE.ask
  pure $ U.getVty (unMetas state) (unLevel context) val

freshMeta :: Elab sig m => N.Value -> m N.Value
freshMeta sig = do
  state <- SE.get
  context <- RE.ask
  cTy <- readback sig -- FIXME: Remove this `readback`
  let meta = C.gen $ C.InsertedMeta (unBinderInfo context) (Global $ nextMeta state) (Just cTy)
  SE.put $ state { nextMeta = nextMeta state + 1 }
  eval meta

freshUnivMeta :: Elab sig m => m N.Value
freshUnivMeta = do
  state <- SE.get
  context <- RE.ask
  let meta = C.gen $ C.InsertedMeta (unBinderInfo context) (Global $ nextMeta state) Nothing
  SE.put $ state { nextMeta = nextMeta state + 1 }
  eval meta