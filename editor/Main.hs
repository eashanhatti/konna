{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Generics.Zipper(Zipper, getHole, setHole, toZipper, fromZipper, query, trans)
import qualified Data.Generics.Zipper as Z
import Surface
import Elaboration.Error
import Elaboration.Effect(runElab)
import qualified Elaboration.Term as ET
import qualified Norm as N
import Control.Algebra(Has, run)
import Control.Monad(forM_)
import qualified Control.Effect.Lift as LE
import Control.Carrier.Lift(runM)
import qualified Control.Effect.State as SE
import Control.Carrier.State.Strict(runState)
import Data.Maybe(fromJust, fromMaybe)
import Data.Data(Data)
import Data.Typeable(cast, typeOf)
import Prelude hiding(Left, Right)
import qualified Prelude
import qualified Data.Text as T
import System.Console.ANSI(clearScreen)
import Foreign.C.Types
import Data.Char(chr)
import System.IO
import qualified Data.Text.IO as TIO
import Etc

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

holeIsList :: Zipper a -> Bool
holeIsList z = case z of
  (getHole -> Just _ :: Maybe [TermAst]) -> True
  (getHole -> Just _ :: Maybe [NameAst]) -> True
  _ -> False

holeIsEmptyList :: Zipper a -> Bool
holeIsEmptyList z = case z of
  (getHole -> Just [] :: Maybe [TermAst]) -> True
  (getHole -> Just [] :: Maybe [NameAst]) -> True
  _ -> False

moveListLast :: Zipper a -> Zipper a
moveListLast z =
  if holeIsEmptyList z then
    fromJust $ Z.left z
  else
    moveListLast (fromJust $ Z.down z)

moveOutList :: Zipper a -> Zipper a
moveOutList z =
  if not $ holeIsList $ fromJust $ Z.up z then
    z
  else
    moveOutList $ fromJust $ Z.up z

fjDown = fromJust . Z.down
fjDown' = fromJust . Z.down'
fjUp = fromJust . Z.up

atomic :: Data a => a -> Bool
atomic f = case f of
  (cast -> Just e :: Maybe TermAst) -> case e of
    TermAst (Var _) -> True
    TermAst U0 -> True
    TermAst U1 -> True
    TermAst Hole -> True
    TermAst _ -> False
  (cast -> Just n :: Maybe NameAst) -> True
  _ -> error $ show $ typeOf f

down :: Zipper a -> Zipper a
down z = (\f -> (f . fjDown) z) $ case z of
  (getHole -> Just e :: Maybe TermAst) -> case e of
    TermAst (Var _) -> fjUp
    TermAst (Lam _ _) -> fjDown
    TermAst (App _ _) -> moveListLast . fjDown
    TermAst (Pi _ _ _) -> fjDown
    TermAst (Arrow _ _) -> fjDown
    TermAst U0 -> fjUp
    TermAst U1 -> fjUp
    TermAst (Code _) -> fjDown
    TermAst (Quote _) -> fjDown
    TermAst (Splice _) -> fjDown
    TermAst Hole -> fjUp
    TermAst (Let _ _) -> fjDown

down' :: Zipper a -> Zipper a
down' z = (\f -> f $ fjDown z) $ case z of
  (getHole -> Just e :: Maybe TermAst) -> case e of
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
    TermAst (Let _ _) -> fjDown' . fjDown'

left :: Zipper a -> Maybe (Zipper a)
left z = case Z.left z of
  Just z' | holeIsList z' -> Just $ moveListLast z'
  Just z' -> Just z'
  Nothing -> Z.up z >>= \z' ->
    if holeIsList z' then
      Z.left z'
    else
      Nothing

right :: Zipper a -> Maybe (Zipper a)
right z = case Z.right z of
  Just z' | holeIsList z' ->
    if holeIsEmptyList z' then
      Z.right (moveOutList z')
    else
      Z.down z'
  z' -> z'

up :: Zipper a -> Maybe (Zipper a)
up z = case Z.up z of
  Just z' | holeIsList z' -> up z'
  Just z' | Just _ :: Maybe Term <- getHole z' -> up z'
  z' -> z'

handleLeft :: State -> State
handleLeft (State z d) = case (query atomic z, d) of
  (_, Left) -> case left z of
    Just z -> State z Right
    Nothing -> State (up z |> z) Left
  (True, Right) -> State z Left
  (False, Right) -> State (down z) Right

handleRight :: State -> State
handleRight (State z d) = case (query atomic z, d) of
  (_, Right) -> case right z of
    Just z -> State z Left
    Nothing -> State (up z |> z) Right
  (True, Left) -> State z Right
  (False, Left) -> State (down' z) Left

handleInput :: State -> Command -> State
handleInput state@(State z d) cmd = case cmd of
  Move Left -> handleLeft state
  Move Right -> handleRight state
  InsertTerm e -> State (setHole e z) d
  InsertItem i -> State (setHole i z) d
  SetName s -> case z of
    (getHole -> Just _ :: Maybe NameAst) -> State (setHole (NameAst s) z) d
    (getHole -> Just _ :: Maybe TermAst) -> State (setHole (TermAst $ Var s) z) d
    _ -> state

type Render sig m = Has (SE.State [Error]) sig m

render :: State -> TermAst -> ([Error], T.Text)
render state ast = run . runState [] $ renderTerm ast where
  renderTerm :: Render sig m => TermAst -> m T.Text
  renderTerm = \case
    FocusedAst Left ast -> combine [yellowM "{", renderTerm ast, yellowM "]"]
    FocusedAst Right ast -> combine [yellowM "[", renderTerm ast, yellowM "}"]
    ErrorAst errs ast -> do
      errs' <- SE.get
      SE.put (errs ++ errs')
      combine [redM "[", renderTerm ast, redM "]"]
    TermAst (Var (UserName name)) -> pure $ T.pack name
    TermAst (Lam names body) -> combine [pure "λ", T.intercalate " " <$> mapM renderName names, pure ". ", renderTerm body]
    TermAst (App lam args) -> T.intercalate " " <$> mapM renderTerm (lam:args)
    TermAst (Pi name inTy outTy) ->
      combine
        [ pure "("
        , renderName name
        , pure " : "
        , renderTerm inTy
        , pure ") -> "
        , renderTerm outTy ]
    TermAst (Arrow inTy outTy) -> combine [renderTerm inTy, pure " ~> ", renderTerm outTy]
    TermAst U0 -> blueM "U0"
    TermAst U1 -> blueM "U1"
    TermAst (Code ty) -> combine [blueM "Code ", renderTerm ty]
    TermAst (Quote term) -> combine [pure "<", renderTerm term, pure ">"]
    TermAst (Splice term) -> combine [pure "~", renderTerm term]
    TermAst (Let bindings body) ->
      combine
        [ greenM "let"
        , indentForced . T.intercalate "\n" <$> mapM renderItem bindings
        , greenM "\nin"
        , indentForced <$> renderTerm body ]
    TermAst Hole -> pure "?"
    _ -> error $ show ast
  renderItem :: Render sig m => ItemAst -> m T.Text
  renderItem = \case
    FocusedAst Left ast -> combine [yellowM "{", renderItem ast, yellowM "]"]
    FocusedAst Right ast -> combine [yellowM "[", renderItem ast, yellowM "}"]
    ItemAst (TermDef _ name sig def) ->
      combine
        [ pure "val "
        , renderName name
        , pure " : "
        , renderTerm sig
        , pure " = "
        , renderTerm def]
    ItemAst (IndDef _ name sig cs) ->
      combine
        [ greenM "datatype"
        , renderName name
        , pure " : "
        , renderTerm sig
        , indentForced . T.intercalate "\n" <$> mapM renderConstr cs]
  renderConstr :: Render sig m => ConstructorAst -> m T.Text
  renderConstr = \case
    FocusedAst Left ast -> combine [yellowM "{", renderConstr ast, yellowM "]"]
    FocusedAst Right ast -> combine [yellowM "[", renderConstr ast, yellowM "}"]
    ConstructorAst (Constructor _ name sig) -> combine [renderName name, pure " : ", renderTerm sig]
  renderName :: Render sig m => NameAst -> m T.Text
  renderName = \case
    FocusedAst Left ast -> combine [yellowM "{", renderName ast, yellowM "]"]
    FocusedAst Right ast -> combine [yellowM "[", renderName ast, yellowM "}"]
    NameAst (UserName name) -> pure $ T.pack name
  combine :: Render sig m => [m T.Text] -> m T.Text
  combine = \case
    [] -> pure ""
    c:cs -> do
      t <- c
      t' <- combine cs
      pure $ t <> t'
  indentForced :: T.Text -> T.Text
  indentForced s = (if s == "" then "" else "\n") <> (T.intercalate "\n" $ map ("  "<>) (T.lines s))

red s = "\ESC[31;1m" <> s <> "\ESC[39m"
green s = "\ESC[32;1m" <> s <> "\ESC[39m"
purple s = "\ESC[35;1m" <> s <> "\ESC[39m"
yellow s = "\ESC[33;1m" <> s <> "\ESC[39m"
blue s = "\ESC[36;1m" <> s <> "\ESC[39m"
redM :: Render sig m => T.Text -> m T.Text
redM = pure . red
greenM :: Render sig m => T.Text -> m T.Text
greenM = pure . green
purpleM :: Render sig m => T.Text -> m T.Text
purpleM = pure . purple
yellowM :: Render sig m => T.Text -> m T.Text
yellowM = pure . yellow
blueM :: Render sig m => T.Text -> m T.Text
blueM = pure . blue

parseCommand :: String -> State -> Maybe Command
parseCommand s (State _ d) = case s of
  ";q" -> Just Quit
  "\\" -> Just (InsertTerm $ TermAst $ Lam [NameAst $ UserName "_"] (TermAst Hole))
  "(" -> Just (InsertTerm $ TermAst $ App (TermAst Hole) [TermAst Hole])
  " " -> Just (Add d)
  "]" -> Just (Move Right)
  "[" -> Just (Move Left)
  "val " -> Just (InsertItem $ ItemAst $ TermDef (mempty, mempty) (NameAst $ UserName "_") (TermAst Hole) (TermAst Hole))
  "prod " -> Just (InsertItem $ ItemAst $ ProdDef (mempty, mempty) (NameAst $ UserName "_") (TermAst Hole) (NameAst $ UserName "_") [])
  "ind " -> Just (InsertItem $ ItemAst $ IndDef (mempty, mempty) (NameAst $ UserName "_") (TermAst Hole) [])
  "/" -> Just (InsertTerm $ TermAst $ Pi (NameAst $ UserName "_") (TermAst Hole) (TermAst Hole))
  "~> " -> Just (InsertTerm $ TermAst $ Arrow (TermAst Hole) (TermAst Hole))
  "let " -> Just (InsertTerm $ TermAst $ Let [ItemAst $ TermDef (mempty, mempty) (NameAst $ UserName "_") (TermAst Hole) (TermAst Hole)] (TermAst Hole))
  "u0 " -> Just (InsertTerm $ TermAst U0)
  "u1 " -> Just (InsertTerm $ TermAst U1)
  "Code " -> Just (InsertTerm $ TermAst $ Code (TermAst Hole))
  "<" -> Just (InsertTerm $ TermAst $ Quote (TermAst Hole))
  "~ " -> Just (InsertTerm $ TermAst $ Splice (TermAst Hole))
  "case " -> Just (InsertTerm $ TermAst $ Match [ClauseAst $ Clause (PatternAst $ BindingPat (NameAst $ UserName "_")) (TermAst Hole)])
  _ | last s == ' ' -> Just (SetName $ UserName $ init s)
  _ -> Nothing
  where
    split :: String -> String -> Char -> [String]
    split s acc d = case s of
      [] -> [acc]
      c:cs ->
        if c == d then
          acc : split cs "" d
        else
          split cs (acc ++ [c]) d

-- Lol just Ctrl+C + Ctrl+V from StackOverflow. `hSetBuffering stdin NoBuffering` doesn't work on Windows.
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

getCommand :: String -> State -> IO Command
getCommand acc state = do
  putStr "\ESC[2K"
  putStr "\ESC[1000D"
  putStr (reverse acc)
  hFlush stdout
  c <- getHiddenChar
  case c of
    '\b' ->
      if null acc then
        pure Delete
      else
        getCommand (tail acc) state
    _ -> case parseCommand (reverse $ c:acc) state of
      Just cmd -> pure cmd
      Nothing -> getCommand (c:acc) state

insertFocusMarker :: Data a => Direction -> a -> a
insertFocusMarker d f = case f of
  (cast -> Just i :: Maybe ItemAst) -> fromJust $ cast (FocusedAst d i)
  (cast -> Just e :: Maybe TermAst) -> fromJust $ cast (FocusedAst d e)
  (cast -> Just n :: Maybe NameAst) -> fromJust $ cast (FocusedAst d n)
  _ -> error $ show $ typeOf f

loop :: Edit sig m => State -> m ()
loop state@(State z d) = do
  LE.sendIO clearScreen
  let prog = fromZipper (trans (insertFocusMarker d) z)
  let (_, Prelude.Right (st, prog')) = runElab $ ET.check prog (N.gen N.TypeType1)
  let (es, s) = render state prog'
  LE.sendIO $ TIO.putStrLn s
  LE.sendIO $ mapM print es
  cmd <- LE.sendIO $ getCommand "" state
  case cmd of
    Quit -> pure ()
    _ -> loop (handleInput state cmd)

main = runM @IO $ loop (State (toZipper $ TermAst (Var (UserName "foo"))) Left)