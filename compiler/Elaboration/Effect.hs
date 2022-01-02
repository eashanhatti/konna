{-# LANGUAGE ConstraintKinds #-}

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
import Prelude hiding (lookup)

data State = State
  { unErrors :: [Error]
  , unMetas :: N.Metas }
  deriving Show

data Context = Context
  { unLocals :: N.Locals
  , unGlobals :: N.Globals
  , unVarTypes :: Map S.Name (Map N.Value Index)
  , unLevel :: Level
  , unUniv :: N.Value }

type Elab sig m = (Has (RE.Reader Context) sig m, Has (SE.State State) sig m, Has (EE.Error ()) sig m)

getErrors :: Elab sig m => m [Error]
getErrors = do
  state <- SE.get
  SE.put $ state { unErrors = [] }
  pure $ unErrors state

getVarTypes :: Elab sig m => S.Name -> m (Maybe (Map N.Value Index))
getVarTypes name = lookup name . unVarTypes <$> RE.ask

getUniv :: Elab sig m => m N.Value
getUniv = unUniv <$> RE.ask

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

closureToValue :: Elab sig m => N.Closure -> N.Value -> m N.Value
closureToValue closure ty = do
  state <- SE.get
  context <- RE.ask
  pure $ runReader (N.appClosure closure (N.gen $ N.StuckRigidVar ty (unLevel context) [])) (unLevel context, unMetas state, unLocals context, unGlobals context)

bind :: Elab sig m => S.Name -> N.Value -> m a -> m a
bind name ty act = do
  context <- RE.ask
  let entry = fromMaybe mempty (lookup name (unVarTypes context))
  RE.local
    (const $ context
      { unLocals = (N.gen $ N.StuckRigidVar ty (unLevel context) []):(unLocals context)
      , unLevel = incLevel (unLevel context)
      , unVarTypes = insert name (insert ty (Index 0) entry) (unVarTypes context) })
    act

failElab :: Elab sig m => m a
failElab = EE.throwError ()

onFail :: Elab sig m => m a -> m a -> m a
onFail act e = act `EE.catchError` (\() -> e)

readback :: Elab sig m => N.Value -> m C.Term
readback val = do
  state <- SE.get
  context <- RE.ask
  pure $ runReader (N.readback val) (unLevel context, unMetas state, unLocals context, unGlobals context)