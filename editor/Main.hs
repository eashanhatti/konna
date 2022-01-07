{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Generics.Zipper(Zipper, getHole, setHole, toZipper, fromZipper, query, trans)
import qualified Data.Generics.Zipper as Z
import Surface
import Elaboration.Error
import Elaboration.Effect(runElab)
import qualified Elaboration.Term as ET
import qualified Norm as N
import Control.Algebra(Has)
import qualified Control.Effect.Lift as LE
import Control.Carrier.Lift(runM)
import qualified Control.Effect.State as SE
import Data.Maybe(fromJust, fromMaybe)
import Data.Data(Data)
import Data.Typeable(cast)
import Prelude hiding(Left, Right)
import qualified Prelude
import qualified Data.Text as T
import System.Console.ANSI(clearScreen)
import qualified Data.Text.IO as TIO

data State = State
  { unZipper :: Zipper TermAst
  , unSide   :: Direction }

data Command
  = Move Direction
  | HardMove Direction
  | InsertTerm TermAst
  | InsertItem ItemAst
  | Delete
  | Add Direction
  | SetName Name
  | Quit
  deriving Show

type Edit sig m = Has (LE.Lift IO) sig m

(|>) = flip fromMaybe

holeIsList :: forall a b. Data b => Zipper a -> Bool
holeIsList z = case z of
  (getHole -> Just _ :: Maybe [b]) -> True
  _ -> False

holeIsEmptyList :: forall a b. Data b => Zipper a -> Bool
holeIsEmptyList z = case z of
  (getHole -> Just [] :: Maybe [b]) -> True
  _ -> False

moveListLast :: forall a b. Data b => Zipper a -> Zipper a
moveListLast z =
  if holeIsEmptyList @a @b z then
    fromJust $ Z.left z
  else
    moveListLast @a @b (fromJust $ Z.down z)

moveOutList :: forall a b. Data b => Zipper a -> Zipper a
moveOutList z =
  if not $ holeIsList @a @b $ fromJust $ Z.up z then
    z
  else
    moveOutList @a @b $ fromJust $ Z.up z

fjDown = fromJust . Z.down
fjDown' = fromJust . Z.down'
fjUp = fromJust . Z.up

atomic :: Ast a -> Bool
atomic ast = case ast of
  TermAst (Var _) -> True
  TermAst U0 -> True
  TermAst U1 -> True
  TermAst Hole -> True
  TermAst _ -> False

down :: forall a b. Data b => Zipper a -> Zipper a
down z = (\f -> (f . fjDown) z) $ case (fromJust $ getHole z) :: Ast b of
  TermAst (Var _) -> fjUp
  TermAst (Lam _ _) -> fjDown
  TermAst (App _ _) -> moveListLast @a @b . fjDown
  TermAst (Pi _ _ _) -> fjDown
  TermAst (Arrow _ _) -> fjDown
  TermAst U0 -> fjUp
  TermAst U1 -> fjUp
  TermAst (Code _) -> fjDown
  TermAst (Quote _) -> fjDown
  TermAst (Splice _) -> fjDown
  TermAst Hole -> fjUp

down' :: forall a b. Data b => Zipper a -> Zipper a
down' z = (\f -> f $ fjDown z) $ case (fromJust $ getHole z) :: Ast b of
  TermAst (Var _) -> fjUp
  TermAst (Lam _ _) -> fjDown' . fjDown'
  TermAst (App _ _) -> fjDown'
  TermAst (Pi _ _ _) -> fjDown'
  TermAst (Arrow _ _) -> fjDown'
  TermAst U0 -> fjUp
  TermAst U1 -> fjUp
  TermAst (Code _) -> fjDown
  TermAst (Quote _) -> fjDown
  TermAst (Splice _) -> fjDown
  TermAst Hole -> fjUp

left :: forall a b. Data b => Zipper a -> Maybe (Zipper a)
left z = case Z.left z of
  Just z' | Just _ :: Maybe [b] <- getHole z' -> Just $ moveListLast @a @b z'
  Just z' -> Just z'
  Nothing -> Z.up z >>= \z' ->
    if holeIsList @a @b z' then
      Z.left z'
    else
      Nothing

right :: forall a b. Data b => Zipper a -> Maybe (Zipper a)
right z = case Z.right z of
  Just z' | Just _ :: Maybe [b] <- getHole z' ->
    if holeIsEmptyList @a @b z' then
      Z.right (moveOutList @a @b z')
    else
      Z.down z'
  z' -> z'

up :: forall a b. Data b => Zipper a -> Maybe (Zipper a)
up z = case Z.up z of
  Just z' | Just _ :: Maybe [b] <- getHole z' -> Z.up z'
  z' -> z'

handleLeft :: forall a. Data a => State -> State
handleLeft (State z d) = case (atomic (fromJust $ cast z :: Ast a), d) of
  (_, Left) -> case left @TermAst @a z of
    Just z -> State z Right
    Nothing -> State (up @TermAst @a z |> z) Left
  (True, Right) -> State z Left
  (False, Right) -> State (down @TermAst @a z) Right

handleRight :: forall a. Data a => State -> State
handleRight (State z d) = case (atomic (fromJust $ cast z :: Ast a), d) of
  (_, Right) -> case right @TermAst @a z of
    Just z -> State z Left
    Nothing -> State (up @TermAst @a z |> z) Right
  (True, Left) -> State z Right
  (False, Left) -> State (down' @TermAst @a z) Left

handleInput :: forall a. Data a => State -> Command -> State
handleInput state@(State z d) cmd = case cmd of
  Move Left -> handleLeft @a state
  Move Right -> handleRight @a state
  InsertTerm e -> State (setHole e z) d
  InsertItem i -> State (setHole i z) d

type Render sig m = Has (SE.State [Error]) sig m

render :: State -> Ast a -> (T.Text, [Error])
render state ast = case ast of
  TermAst (Var (UserName name)) -> (T.pack name, [])

loop :: Edit sig m => State -> m ()
loop state@(State z d) = do
  LE.sendIO clearScreen
  let prog = fromZipper z
  let (_, Prelude.Right (_, prog')) = runElab $ ET.check prog (N.gen N.TypeType1)
  let (s, _) = render state prog'
  LE.sendIO $ TIO.putStrLn s

main = runM @IO $ loop (State (toZipper $ TermAst (Var (UserName "foo"))) Left)