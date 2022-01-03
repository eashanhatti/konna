{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Elaboration.Effect where

import Control.Algebra(Has)
import qualified Control.Effect.State as SE
import qualified Control.Effect.Reader as RE
import qualified Control.Effect.Error as EE
import Elaboration.Error
import Control.Monad.Reader(runReader)
import qualified Norm as N
import qualified Surface as S
import qualified Unification as U
import qualified Core as C
import Data.Map(Map, lookup, insert, singleton)
import Data.Set(Set)
import Data.Maybe(fromMaybe)
import Var hiding(unLevel)
import Numeric.Natural
import Prelude hiding (lookup)

data State = State
  { unErrors :: [Error]
  , unMetas :: N.Metas
  , nextMeta :: Int }
  deriving Show

data Context = Context
  { unLocals :: N.Locals
  , unVarTypes :: Map S.Name (Map N.Value Index)
  , unLevel :: Level
  , binderInfo :: [C.BinderInfo] }

type Elab sig m = (Has (RE.Reader Context) sig m, Has (SE.State State) sig m, Has (EE.Error ()) sig m)

getErrors :: Elab sig m => m [Error]
getErrors = do
  state <- SE.get
  SE.put $ state { unErrors = [] }
  pure $ unErrors state

getVarTypes :: Elab sig m => S.Name -> m (Map N.Value Index)
getVarTypes name = lookup name . unVarTypes <$> RE.ask >>= \case
  Just tys -> pure tys
  Nothing -> pure mempty

unify :: Elab sig m => N.Value -> N.Value -> m [U.Error]
unify val val' = do
  state <- SE.get
  context <- RE.ask
  let ((), (newMetas, errors)) = U.runUnify (U.unify (unLevel context) val val') (unMetas state, [])
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
closureToValue closure ty = do
  state <- SE.get
  context <- RE.ask
  pure $ runReader (N.appClosure closure (N.gen $ N.StuckRigidVar ty (unLevel context) [])) (unLevel context, unMetas state, unLocals context)

bind :: Elab sig m => S.Name -> N.Value -> m a -> m a
bind name ty act = do
  context <- RE.ask
  let entry = fromMaybe mempty (lookup name (unVarTypes context))
  RE.local
    (const $ context
      { unLocals = (N.gen $ N.StuckRigidVar ty (unLevel context) []):(unLocals context)
      , unLevel = incLevel (unLevel context)
      , unVarTypes = insert name (insert ty (Index 0) entry) (unVarTypes context)
      , binderInfo = C.Abstract:(binderInfo context) })
    act

bindUnnamed :: Elab sig m => N.Value -> m a -> m a
bindUnnamed ty act = do
  context <- RE.ask
  RE.local
    (const $ context
      { unLocals = (N.gen $ N.StuckRigidVar ty (unLevel context) []):(unLocals context)
      , unLevel = incLevel (unLevel context)
      , binderInfo = C.Abstract:(binderInfo context) })
    act

failElab :: Elab sig m => m a
failElab = EE.throwError ()

onFail :: Elab sig m => m a -> m a -> m a
onFail act e = act `EE.catchError` (\() -> e)

readback :: Elab sig m => N.Value -> m C.Term
readback val = do
  state <- SE.get
  context <- RE.ask
  pure $ runReader (N.readback val) (unLevel context, unMetas state, unLocals context)

eval :: Elab sig m => C.Term -> m N.Value
eval term = do
  state <- SE.get
  context <- RE.ask
  pure $ runReader (N.eval term) (unLevel context, unMetas state, unLocals context)

typeOf :: Elab sig m => N.Value -> m N.Value
typeOf val = do
  state <- SE.get
  context <- RE.ask
  pure $ U.getVty (unMetas state) (unLevel context) val

freshMeta :: Elab sig m => N.Value -> m N.Value
freshMeta ty = do
  state <- SE.get
  context <- RE.ask
  cTy <- readback ty -- FIXME: Remove this `readback`
  let meta = C.gen $ C.InsertedMeta (binderInfo context) (Global $ nextMeta state) (Just cTy)
  SE.put $ state { nextMeta = nextMeta state + 1 }
  eval meta

freshUnivMeta :: Elab sig m => m N.Value
freshUnivMeta = do
  state <- SE.get
  context <- RE.ask
  let meta = C.gen $ C.InsertedMeta (binderInfo context) (Global $ nextMeta state) Nothing
  SE.put $ state { nextMeta = nextMeta state + 1 }
  eval meta