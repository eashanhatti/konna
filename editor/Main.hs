{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- import Data.Text
-- import Data.Text.IO(putStrLn)
-- import Prelude hiding(putStrLn)
import TextShow
import TextShow.TH
import Surface
import System.Console.ANSI
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get(runGet)
import qualified Data.Map as DM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LB
import Data.List(intersperse)
import Prelude hiding (Left, Right)
import Parsing(getItem)
import System.IO
import GHC.IO.Encoding
import Data.Char
import Foreign.C.Types
import Data.Bifunctor
import Control.Monad(forM_)
import Control.Effect.State(State)
import qualified Control.Effect.State as SE
import Control.Effect.Lift(Lift, Has)
import qualified Control.Effect.Lift as LE
import Control.Carrier.State.Strict(runState)
import Control.Carrier.Lift(runM)
import qualified Elaboration as Elab
import qualified Core as C
import Debug.Trace(traceShowId)

data Con = Con String Term | EditorBlankCon
  deriving (Show, Eq)

unCon (Con n t) = (n, t)
conPair (n, t) = Con n t

data Path a where
  PTop                 :: Path Item
  PTermDefName         :: Path Item -> Term -> Term -> Path String
  PTermDefBody         :: Path Item -> String -> Term -> Path Term
  PTermDefTy           :: Path Item -> String -> Term -> Path Term
  PNamespaceDefName    :: Path Item -> [Item] -> Path String
  PNamespaceDefItems   :: Path Item -> String -> [Item] -> [Item] -> Path Item
  PIndDefName          :: Path Item -> Term -> [Con] -> Path String
  PIndDefTy            :: Path Item -> String -> [Con] -> Path Term
  PIndDefCons          :: Path Item -> String -> Term -> [Con] -> [Con] -> Path Con
  PConName             :: Path Con  -> Term -> Path String
  PConTy               :: Path Con  -> String -> Path Term
  PProdDefName         :: Path Item -> Term -> [Term] -> Path String
  PProdDefTy           :: Path Item -> String -> [Term] -> Path Term
  PProdDefFields       :: Path Item -> String -> Term -> [Term] -> [Term] -> Path Term
  PLamParams           :: Path Term -> [String] -> [String] -> Term -> Path String
  PLamBody             :: Path Term -> [String] -> Path Term
  PAppTerms            :: Path Term -> [Term] -> [Term] -> Path Term
  PLetName             :: Path Term -> Term -> Term -> Term -> Path String
  PLetDefTy            :: Path Term -> String -> Term -> Term -> Path Term
  PLetDef              :: Path Term -> String -> Term -> Term -> Path Term
  PLetBody             :: Path Term -> String -> Term -> Term -> Path Term
  PMkProdTy            :: Path Term -> [Term] -> Path Term
  PMkProdArgs          :: Path Term -> Term -> [Term] -> [Term] -> Path Term
  PPiName              :: Path Term -> Term -> Term -> Path String
  PPiIn                :: Path Term -> String -> Term -> Path Term
  PPiOut               :: Path Term -> String -> Term -> Path Term
  PCode                :: Path Term -> Path Term
  PQuote               :: Path Term -> Path Term
  PSplice              :: Path Term -> Path Term
deriving instance Show (Path a)
deriving instance Eq (Path a)

data Focus a where
  FName :: String -> Focus String
  FTerm :: Term -> Focus Term
  FItem :: Item -> Focus Item
  FCon  :: Con -> Focus Con
deriving instance Show (Focus a)
deriving instance Eq (Focus a)

unFName :: Focus String -> String
unFName (FName s) = s
unFTerm :: Focus Term -> Term
unFTerm (FTerm e) = e
unFItem :: Focus Item -> Item
unFItem (FItem i) = i
unFCon :: Focus Con -> Con
unFCon  (FCon c)  = c

data FocusType a where
  FTName :: FocusType String
  FTTerm :: FocusType Term
  FTItem :: FocusType Item
  FTCon  :: FocusType Con
deriving instance Eq (FocusType a)
deriving instance Show (FocusType a)

data Cursor a = Cursor { unFocus :: Focus a, unPath :: Path a }
deriving instance Show (Cursor a)
deriving instance Eq (Cursor a)

data EditorState a = EditorState { unCursor :: Cursor a, unFocusType :: FocusType a, unSide :: Direction }
deriving instance Eq (EditorState String)
deriving instance Eq (EditorState Term)
deriving instance Eq (EditorState Item)
deriving instance Eq (EditorState Con)
deriving instance Show (EditorState a)

statesEq :: EditorState a -> EditorState b -> Bool
statesEq st st' = case (unFocusType st, unFocusType st') of
  (FTName, FTName) -> st == st'
  (FTTerm, FTTerm) -> st == st'
  (FTItem, FTItem) -> st == st'
  (FTCon, FTCon) -> st == st'
  _ -> False

data Ex = forall a. Ex { unEx :: EditorState a }

type Edit sig m = (Has (State Changes) sig m, Has (State GName) sig m, Has (State (Maybe ItemPart)) sig m)
type EditIO sig m = (Has (Lift IO) sig m, Edit sig m)

data Command a where
  InsertTerm         :: TermInsertionType -> Command Term
  InsertVar          :: String -> Command Term
  InsertHole         :: Command Term
  InsertU0           :: Command Term
  InsertU1           :: Command Term
  InsertTermDef      :: Command Item
  InsertNamespaceDef :: Command Item
  InsertIndDef       :: Command Item
  InsertProdDef      :: Command Item
  InsertGVar         :: [String] -> Command Term
  InsertCon          :: String -> Command Con
  SetName            :: String -> Command String
  MoveOut            :: Direction -> Command a
  MoveRight          :: Command a
  MoveLeft           :: Command a
  MoveInLeft         :: Command a
  MoveInRight        :: Command a
  Add                :: Command a
  Delete             :: Command a

class MkFT a where focusType :: FocusType a
instance MkFT Term where   focusType = FTTerm
instance MkFT String where focusType = FTName
instance MkFT Item where   focusType = FTItem
instance MkFT Con where    focusType = FTCon

class MkFocus a where focus :: a -> Focus a
instance MkFocus Term where   focus = FTerm
instance MkFocus Item where   focus = FItem
instance MkFocus String where focus = FName
instance MkFocus Con where    focus = FCon

type Changes = DM.Map GName ItemPart

mkEx :: (MkFT a, MkFocus a) => a -> Path a -> Direction -> Ex
mkEx f p s = Ex $ EditorState (Cursor (focus f) p) focusType s

mkExE :: (MkFT a, MkFocus a, Edit sig m) => a -> Path a -> Direction -> m Ex
mkExE f p s = pure $ mkEx f p s

blankFocus :: Focus a -> Bool
blankFocus focus = case focus of
  FTerm EditorBlank -> True
  FItem EditorBlankDef -> True
  FName "" -> True
  FCon EditorBlankCon -> True
  _ -> False

popName :: Has (State GName) sig m => m ()
popName = do
  GName ns <- SE.get
  SE.put (GName $ tail ns)

pushName :: Has (State GName) sig m => String -> m ()
pushName n = do
  GName ns <- SE.get
  SE.put (GName $ n:ns)

putPart :: Has (State (Maybe ItemPart)) sig m => ItemPart -> m ()
putPart part = SE.put @(Maybe ItemPart) (Just part)

clearPart :: Has (State (Maybe ItemPart)) sig m => m ()
clearPart = SE.put @(Maybe ItemPart) Nothing

markChange :: Edit sig m => m ()
markChange = do
  part <- SE.get @(Maybe ItemPart)
  case part of
    Just part -> do
      changes <- SE.get @Changes
      name <- SE.get @GName
      case DM.lookup name changes of
        Just Dec -> pure ()
        _ -> SE.put @Changes (DM.insert name part changes)
    Nothing -> pure ()

itemName :: Path a -> Focus a -> Maybe String
itemName path focus = case path of
  PTermDefName _ _ _ -> Just $ unFName focus
  PTermDefBody _ name _ -> Just name
  PTermDefTy _ name _ -> Just name
  PNamespaceDefName _ _ -> Just $ unFName focus
  PNamespaceDefItems _ name _ _ -> Just name
  PIndDefName _ _ _ -> Just $ unFName focus
  PIndDefTy _ name _ -> Just name
  PIndDefCons _ name _ _ _ -> Just name
  PConName _ _ -> Just $ unFName focus
  PConTy _ name -> Just name
  PProdDefName _ _ _ -> Just $ unFName focus
  PProdDefTy _ name _ -> Just name
  PProdDefFields _ name _ _ _ -> Just name
  _ -> Nothing

run :: Edit sig m => Command a -> EditorState a -> m Ex
run command state@(EditorState (Cursor focus path) focusType side) = do
  state' <- case command of
    InsertTerm ti -> do
      markChange
      case ti of
        TILam -> mkExE (Lam [Name "_"] termFocus) path Left
        TIApp -> mkExE (App termFocus [Hole]) path Left
        TILet -> mkExE (Let (Name "_") Hole Hole termFocus) path Left
        TIPi -> mkExE (Pi (Name "_") termFocus Hole) path Left
        TICode -> mkExE (Code termFocus) path Left
        TIQuote -> mkExE (Quote termFocus) path Left
        TISplice -> mkExE (Splice termFocus) path Left
        TIMkProd -> mkExE (MkProd termFocus []) path Left
      where
        termFocus = unFTerm focus
    InsertVar s -> markChange >> mkExE (Var (Name s)) path Left
    InsertHole -> markChange >> mkExE Hole path Left
    InsertTermDef -> markItemInsertion >> mkExE (TermDef (Name "_") Hole Hole) path Left
    InsertNamespaceDef -> mkExE (NamespaceDef (Name "_") []) path Left
    InsertIndDef -> markItemInsertion >> mkExE (IndDef (Name "_") Hole []) path Left
    InsertU0 -> markChange >> mkExE U0 path Left
    InsertU1 -> markChange >> mkExE U1 path Left
    InsertGVar ns -> markChange >> mkExE (GVar $ GName ns) path Left
    InsertCon s -> markItemInsertion >> mkExE Hole (PConTy path s) Left
    InsertProdDef -> markItemInsertion >> mkExE (ProdDef (Name "_") Hole []) path Left
    SetName s -> do
      case itemName path focus of
        Just _ -> do
          part <- SE.get @(Maybe ItemPart)
          putPart Dec
          markChange
          case part of
            Just part -> putPart part
            Nothing -> clearPart
          popName
          pushName s
        Nothing -> pure ()
      mkExE s path Left
    Add ->
      if blankFocus focus then
        pure oldState
      else case path of
        PLamParams up ln rn body -> mkExE "" (PLamParams up (insertFocusR focus ln) rn body) Left
        PAppTerms up le re -> mkExE EditorBlank (PAppTerms up (insertFocusR focus le) re) Left
        PNamespaceDefItems up name li ri -> mkExE EditorBlankDef (PNamespaceDefItems up name (insertFocusR focus li) ri) Left
        PIndDefCons up name ty lc rc -> mkExE EditorBlankCon (PIndDefCons up name ty (insertFocusR focus lc) rc) Left
        PProdDefFields up name ty lf rf -> mkExE EditorBlank (PProdDefFields up name ty (insertFocusR focus lf) rf) Left
        PMkProdArgs up ty le re -> mkExE EditorBlank (PMkProdArgs up ty (insertFocusR focus le) re) Left
        _ -> pure oldState
    MoveRight -> case path of
      PTop -> pure sideRight
      PLamParams up ln [] body -> mkExE body (PLamBody up (insertFocusR focus ln)) Left
      PLamParams up ln (n:rn) body -> mkExE n (PLamParams up (insertFocusR focus ln) rn body) Left
      PLamBody up ns -> pure sideRight
      PAppTerms up le [] -> pure sideRight
      PAppTerms up le (r:re) -> mkExE r (PAppTerms up (insertFocusR focus le) re) Left
      PLetName up def defTy body -> mkExE defTy (PLetDefTy up (unFName focus) def body) Left
      PLetDefTy up name def body -> mkExE def (PLetDef up name (unFTerm focus) body) Left
      PLetDef up name defTy body -> mkExE body (PLetBody up name (unFTerm focus) defTy) Left
      PMkProdTy up [] -> mkExE EditorBlank (PMkProdArgs up (unFTerm focus) [] []) Left
      PMkProdTy up (e:es) -> mkExE e (PMkProdArgs up (unFTerm focus) [] es) Left
      PMkProdArgs up ty le [] -> pure sideRight
      PMkProdArgs up ty le (r:re) -> mkExE r (PMkProdArgs up ty (insertFocusR focus le) re) Left    
      PLetBody _ _ _ _ -> pure sideRight
      PTermDefName up ty body -> putPart Dec >> mkExE ty (PTermDefTy up (unFName focus) body) Left
      PTermDefTy up name body -> putPart Def >> mkExE body (PTermDefBody up name (unFTerm focus)) Left
      PTermDefBody _ _ _ -> pure sideRight
      PNamespaceDefName up [] -> mkExE EditorBlankDef (PNamespaceDefItems up (unFName focus) [] []) Left
      PNamespaceDefName up (i:is) -> mkExE i (PNamespaceDefItems up (unFName focus) [] is) Left
      PNamespaceDefItems up name _ [] -> pure sideRight
      PNamespaceDefItems up name li (i:ri) -> mkExE i (PNamespaceDefItems up name (insertFocusR focus li) ri) Left
      PPiName up inTy outTy -> mkExE inTy (PPiIn up (unFName focus) outTy) Left
      PPiIn up name outTy -> mkExE outTy (PPiOut up name (unFTerm focus)) Left
      PPiOut _ _ _ -> pure sideRight
      PCode _ -> pure sideRight
      PQuote _ -> pure sideRight
      PSplice _ -> pure sideRight
      PIndDefName up ty cons -> putPart Dec >> mkExE ty (PIndDefTy up (unFName focus) cons) Left
      PIndDefTy up name [] -> clearPart >> mkExE EditorBlankCon (PIndDefCons up name (unFTerm focus) [] []) Left 
      PIndDefTy up name (c:cs) -> clearPart >> mkExE c (PIndDefCons up name (unFTerm focus) [] cs) Left
      PIndDefCons up name ty lc [] -> pure sideRight
      PIndDefCons up name ty lc (c:rc) -> mkExE c (PIndDefCons up name ty (insertFocusR focus lc) rc) Left
      PProdDefName up ty fs -> putPart Dec >> mkExE ty (PProdDefTy up (unFName focus) fs) Left
      PProdDefTy up name [] -> clearPart >> mkExE EditorBlank (PProdDefFields up name (unFTerm focus) [] []) Left
      PProdDefTy up name (f:fs) -> clearPart >> mkExE f (PProdDefFields up name (unFTerm focus) [] fs) Left
      PProdDefFields up name ty lf [] -> pure sideRight
      PProdDefFields up name ty lf (f:rf) -> mkExE f (PProdDefFields up name ty (insertFocusR focus lf) rf) Left
      PConName up ty -> putPart Dec >> mkExE ty (PConTy up (unFName focus)) Left
      PConTy _ _ -> clearPart >> pure sideRight
    MoveLeft -> case path of
      PTop -> pure sideLeft
      PLamParams up [] rn body -> orSideLeft $ mkExE "" (PLamParams up [] (insertFocusL focus rn) body) Left
      PLamParams up ln rn body -> mkExE (last ln) (PLamParams up (init ln) (insertFocusL focus rn) body) Left
      PLamBody up ns -> mkExE (last ns) (PLamParams up (init ns) [] (unFTerm focus)) Left
      PAppTerms up [] re -> orSideLeft $ mkExE EditorBlank (PAppTerms up [] (insertFocusL focus re)) Right
      PAppTerms up le re -> mkExE (last le) (PAppTerms up (init le) (insertFocusL focus re)) Right
      PLetName _ _ _ _ -> pure sideLeft
      PLetDefTy up name def body -> mkExE name (PLetName up def (unFTerm focus) body) Left
      PLetDef up name defTy body -> mkExE defTy (PLetDefTy up name (unFTerm focus) body) Right
      PLetBody up name def defTy -> mkExE def (PLetDef up name defTy (unFTerm focus)) Right
      PMkProdTy _ _ -> pure sideLeft
      PMkProdArgs up ty [] es -> mkExE ty (PMkProdTy up es) Right
      PMkProdArgs up ty le re -> mkExE (last le) (PMkProdArgs up ty (init le) (insertFocusL focus re)) Right
      PTermDefName up ty body -> pure sideLeft
      PTermDefTy up name body -> clearPart >> mkExE name (PTermDefName up (unFTerm focus) body) Left
      PTermDefBody up name ty -> putPart Dec >> mkExE ty (PTermDefTy up name (unFTerm focus)) Right
      PNamespaceDefName up _ -> pure sideLeft
      PNamespaceDefItems up name [] ri -> orSideLeft $ mkExE EditorBlankDef (PNamespaceDefItems up name [] (insertFocusL focus ri)) Right
      PNamespaceDefItems up name li ri -> mkExE (last li) (PNamespaceDefItems up name (init li) (insertFocusL focus ri)) Right
      PPiName _ _ _ -> pure sideLeft
      PPiIn up name outTy -> mkExE name (PPiName up (unFTerm focus) outTy) Left
      PPiOut up name inTy -> mkExE inTy (PPiIn up name (unFTerm focus)) Right
      PCode _ -> pure sideLeft
      PQuote _ -> pure sideLeft
      PSplice _ -> pure sideLeft
      PIndDefName _ _ _ -> pure sideLeft
      PIndDefTy up name cons -> clearPart >> mkExE name (PIndDefName up (unFTerm focus) cons) Left
      PIndDefCons up name ty [] rc -> orSideLeft $ mkExE EditorBlankCon (PIndDefCons up name ty [] (insertFocusL focus rc)) Right
      PIndDefCons up name ty lc rc -> mkExE (last lc) (PIndDefCons up name ty (init lc) (insertFocusL focus rc)) Right
      PProdDefName _ _ _ -> pure sideLeft
      PProdDefTy up name fs -> mkExE name (PProdDefName up (unFTerm focus) fs) Left
      PProdDefFields up name ty [] rf -> orSideLeft $ mkExE EditorBlank (PProdDefFields up name ty [] (insertFocusL focus rf)) Right
      PProdDefFields up name ty lf rf -> mkExE (last lf) (PProdDefFields up name ty (init lf) (insertFocusL focus rf)) Right
      PConName _ _ -> pure sideLeft
      PConTy up name -> clearPart >> mkExE name (PConName up (unFTerm focus)) Left
      where
        orSideLeft :: Edit sig m => m Ex -> m Ex
        orSideLeft ex =
          if blankFocus focus then
            pure sideLeft
          else
            ex
    MoveOut d -> do
      case itemName path focus of
        Just _ -> popName
        Nothing -> pure ()
      case path of
        PTop -> pure oldState
        PLamParams up ln rn body -> mkExE (Lam (map Name ln ++ (map Name $ insertFocusL focus rn)) body) up d
        PLamBody up ns -> mkExE (Lam (map Name ns) (unFTerm focus)) up d
        PAppTerms up le re ->
          let es = le ++ insertFocusL focus re
          in mkExE (App (head es) (tail es)) up d
        PLetName up def defTy body -> mkExE (Let (Name $ unFName focus) def defTy body) up d
        PLetDefTy up name def body -> mkExE (Let (Name name) def (unFTerm focus) body) up d
        PLetDef up name defTy body -> mkExE (Let (Name name) (unFTerm focus) defTy body) up d
        PLetBody up name def defTy -> mkExE (Let (Name name) def defTy (unFTerm focus)) up d
        PMkProdTy up es -> mkExE (MkProd (unFTerm focus) es) up d
        PMkProdArgs up ty le re -> mkExE (MkProd ty (le ++ insertFocusL focus re)) up d
        PTermDefName up ty body -> mkExE (TermDef (Name $ unFName focus) ty body) up d
        PTermDefTy up name body -> clearPart >> mkExE (TermDef (Name name) (unFTerm focus) body) up d
        PTermDefBody up name ty -> clearPart >> mkExE (TermDef (Name name) ty (unFTerm focus)) up d
        PNamespaceDefName up items -> mkExE (NamespaceDef (Name $ unFName focus) items) up d
        PNamespaceDefItems up name li ri -> mkExE (NamespaceDef (Name name) (li ++ insertFocusL focus ri)) up d
        PPiName up inTy outTy -> mkExE (Pi (Name $ unFName focus) inTy outTy) up d
        PPiIn up name outTy -> mkExE (Pi (Name name) (unFTerm focus) outTy) up d
        PPiOut up name inTy -> mkExE (Pi (Name name) inTy (unFTerm focus)) up d
        PCode up -> mkExE (Code $ unFTerm focus) up d
        PQuote up -> mkExE (Quote $ unFTerm focus) up d
        PSplice up -> mkExE (Splice $ unFTerm focus) up d
        PIndDefName up ty cons -> mkExE (IndDef (Name $ unFName focus) ty (map (first Name . unCon) cons)) up d
        PIndDefCons up name ty lc [] ->
          let (Con n t) = unFCon focus
          in mkExE (IndDef (Name name) ty (map (first Name . unCon) lc ++ [(Name n, t)])) up d
        PIndDefTy up name cons -> clearPart >> mkExE (IndDef (Name name) (unFTerm focus) (map (first Name . unCon) cons)) up d
        PIndDefCons up name ty lc rc -> mkExE (IndDef (Name name) ty (map (first Name . unCon) $ lc ++ insertFocusL focus rc)) up d
        PProdDefName up ty fs -> mkExE (ProdDef (Name $ unFName focus) ty fs) up d
        PProdDefTy up name fs -> clearPart >> mkExE (ProdDef (Name name) (unFTerm focus) fs) up d
        PProdDefFields up name ty lf rf -> mkExE (ProdDef (Name name) ty (lf ++ insertFocusL focus rf)) up d
        PConName up ty -> mkExE (Con (unFName focus) ty) up d
        PConTy up name -> clearPart >> mkExE (Con name (unFTerm focus)) up d
    MoveInLeft -> case focus of
      FTerm focus -> case focus of
        Lam (Name n:ns) body -> mkExE n (PLamParams path [] (map unName ns) body) Left
        App lam args -> mkExE lam (PAppTerms path [] args) Left
        Let (Name name) def defTy body -> mkExE name (PLetName path def defTy body) Left
        Pi (Name name) inTy outTy -> mkExE name (PPiName path inTy outTy) Left
        Var _ -> pure oldState
        GVar _ -> pure oldState
        U0 -> pure oldState
        U1 -> pure oldState
        Code ty -> mkExE ty (PCode path) Left
        Quote e -> mkExE e (PQuote path) Left
        Splice e -> mkExE e (PSplice path) Left
        MkProd ty es -> mkExE ty (PMkProdTy path es) Left
        Hole -> pure oldState
        EditorBlank -> pure oldState
      FItem focus -> do
        let
          (n, ex) = case focus of
            TermDef (Name n) ty body -> (n, mkExE n (PTermDefName path ty body) Left)
            NamespaceDef (Name n) items -> (n, mkExE n (PNamespaceDefName path items) Left)
            IndDef (Name n) ty cons -> (n, mkExE n (PIndDefName path ty (map (conPair . first unName) cons)) Left)
            ProdDef (Name n) ty fields -> (n, mkExE n (PProdDefName path ty fields) Left)
        pushName n
        ex
      FCon focus -> case focus of
        Con n t -> do
          pushName n
          mkExE n (PConName path t) Left
        EditorBlankCon -> pure sideLeft
      FName _ -> pure sideLeft
    MoveInRight -> case focus of
      FTerm focus -> case focus of
        Lam ns body -> mkExE body (PLamBody path (map unName ns)) Right
        App lam args -> mkExE (last args) (PAppTerms path (lam : init args) []) Right
        Let (Name name) def defTy body -> mkExE body (PLetBody path name def defTy) Right
        Pi (Name name) inTy outTy -> mkExE outTy (PPiOut path name inTy) Right
        Var _ -> pure oldState
        GVar _ -> pure oldState
        U0 -> pure oldState
        U1 -> pure oldState
        Code ty -> mkExE ty (PCode path) Right
        Quote e -> mkExE e (PQuote path) Right
        Splice e -> mkExE e (PSplice path) Right
        MkProd ty [] -> mkExE EditorBlank (PMkProdArgs path ty [] []) Right
        Hole -> pure oldState
        EditorBlank -> pure oldState
      FItem focus -> do
        let
          (n, ex) = case focus of
            TermDef (Name n) ty body -> (n, putPart Def >> mkExE body (PTermDefBody path n ty) Right)
            NamespaceDef (Name n) [] -> (n, mkExE EditorBlankDef (PNamespaceDefItems path n [] []) Right)
            NamespaceDef (Name n) items -> (n, mkExE (last items) (PNamespaceDefItems path n (init items) []) Right)
            IndDef (Name n) ty [] -> (n, mkExE EditorBlankCon (PIndDefCons path n ty [] []) Right)
            IndDef (Name n) ty cons -> (n, mkExE ((\(Name n, t) -> Con n t) $ last cons) (PIndDefCons path n ty (map (conPair . first unName) $ init cons) []) Right)
            ProdDef (Name n) ty [] -> (n, mkExE EditorBlank (PProdDefFields path n ty [] []) Right)
            ProdDef (Name n) ty fs -> (n, mkExE (last fs) (PProdDefFields path n ty (init fs) []) Right)
        pushName n
        ex
      FCon focus -> case focus of
        Con n t -> do
          pushName n
          mkExE t (PConTy path n) Right
        EditorBlankCon -> pure sideRight
      FName _ -> pure sideRight
    Delete -> case path of
      PNamespaceDefItems up name [] []       -> mkExE name (PNamespaceDefName up []) Left
      PNamespaceDefItems up name li@(_:_) ri -> mkExE (last li) (PNamespaceDefItems up name (init li) ri) Right
      PIndDefCons up name ty [] []           -> markDecChange >> mkExE ty (PIndDefTy up name []) Right
      PIndDefCons up name ty lc@(_:_) rc     -> markDecChange >> mkExE (last lc) (PIndDefCons up name ty (init lc) rc) Right
      PLamParams up [] [] body               -> pure oldState
      PLamParams up [] (n:rn) body           -> markChange >> mkExE n (PLamParams up [] rn body) Left
      PLamParams up ln rn body               -> markChange >> mkExE (last ln) (PLamParams up (init ln) rn body) Right
      PProdDefFields up name ty [] []        -> mkExE ty (PProdDefTy up name []) Right
      PProdDefFields up name ty lf@(_:_) rf  -> mkExE (last lf) (PProdDefFields up name ty (init lf) rf) Right
      PMkProdArgs up ty [] []                -> markChange >> mkExE ty path Right
      PMkProdArgs up ty le@(_:_) re          -> markChange >> mkExE (last le) (PMkProdArgs up ty (init le) re) Right
      PAppTerms up le re -> markChange >> case le ++ re of
        [] -> mkExE Hole path Right
        [e] -> mkExE e path Right
        _ -> mkExE (last le) (PAppTerms up (init le) re) Right
      _ -> case focusType of
        FTTerm -> markChange >> mkExE Hole path Left
        _ -> pure oldState
  pure state'
  where
    oldState = Ex state
    sideRight = case side of
      Left -> Ex $ state { unSide = Right }
      Right -> Ex state
    sideLeft = case side of
      Left -> Ex state
      Right -> Ex $ state { unSide = Left }
    insertFocusR :: Focus a -> [a] -> [a]
    insertFocusR focus la = case focus of
      FTerm EditorBlank -> la
      FTerm _ -> la ++ [unFTerm focus]
      FName "" -> la
      FName _ -> la ++ [unFName focus]
      FCon EditorBlankCon -> la
      FCon _ -> la ++ [unFCon focus]
      FItem EditorBlankDef -> la
      FItem _ -> la ++ [unFItem focus]
    insertFocusL :: Focus a -> [a] -> [a]
    insertFocusL focus la = case focus of
      FTerm EditorBlank -> la
      FTerm _ -> unFTerm focus : la
      FName "" -> la
      FName _ -> unFName focus : la
      FCon EditorBlankCon -> la
      FCon _ -> unFCon focus : la
      FItem EditorBlankDef -> la
      FItem _ -> unFItem focus : la
    markDecChange :: Edit sig m => m ()
    markDecChange = putPart Dec >> markChange >> clearPart
    markItemInsertion :: Edit sig m => m ()
    markItemInsertion = pushName "_" >> markDecChange >> popName

edge :: Direction -> Path a -> Bool
edge d p = case d of
  Left -> case p of
    PTop -> True
    PTermDefName _ _ _ -> True
    PNamespaceDefName _ _ -> True
    PAppTerms _ [] _ -> True
    PIndDefName _ _ _ -> True
    PProdDefName _ _ _ -> True
    PConName _ _ -> True
    PLetName _ _ _ _ -> True
    PPiName _ _ _ -> True
    PMkProdTy _ _ -> True
    PCode _ -> True
    PQuote _ -> True
    PSplice _ -> True
    _ -> False
  Right -> case p of
    PTop -> True
    PTermDefBody _ _ _ -> True
    PNamespaceDefItems _ _ _ [] -> True
    PIndDefCons _ _ _ _ [] -> True
    PProdDefFields _ _ _ _ [] -> True
    PConTy _ _ -> True
    PLamBody _ _ -> True
    PAppTerms _ _ [] -> True
    PLetBody _ _ _ _ -> True
    PPiOut _ _ _ -> True
    PMkProdArgs _ _ _ [] -> True
    PCode _ -> True
    PQuote _ -> True
    PSplice _ -> True
    _ -> False

atomic :: Focus a -> Bool
atomic focus = case focus of
  FTerm term -> case term of
    Hole -> True
    EditorBlank -> True
    Var _ -> True
    GVar _ -> True
    U0 -> True
    U1 -> True
    _ -> False
  FItem item -> case item of
    EditorBlankDef -> True
    _ -> False
  FCon con -> case con of
    EditorBlankCon -> True
    _ -> False
  FName _ -> True

putWord16 :: Word16 -> Put
putWord16 = put

putItem :: Item -> Put
putItem item = case item of
  NamespaceDef (Name n) items -> do
    putWord8 0
    putString n
    putWord16 $ fromIntegral (length items)
    forM_ items putItem
  TermDef (Name n) ty body -> do
    putWord8 1
    putString n
    putTerm ty
    putTerm body
  IndDef (Name n) ty cons -> do
    putWord8 2
    putString n
    putTerm ty
    putWord16 $ fromIntegral (length cons)
    forM_ cons \(Name n, t) -> do
      putString n
      putTerm t
  ProdDef (Name n) ty fields -> do
    putWord8 3
    putString n
    putTerm ty
    putWord16 $ fromIntegral (length fields)
    forM_ fields \field -> putTerm field

putString :: String -> Put
putString s = do
  putWord16 $ fromIntegral (length s)
  forM_ s put

putStrings :: [String] -> Put
putStrings ss = forM_ ss putString

putTerm :: Term -> Put
putTerm term = case term of
  Var (Name name) -> do
    putWord8 0
    putString name
  GVar (GName name) -> do
    putWord8 1
    putWord16 $ fromIntegral (length name)
    putStrings name
  Lam names body -> do
    putWord8 2
    putWord16 $ fromIntegral (length names)
    putStrings (map unName names)
    putTerm body
  App lam args -> do
    putWord8 3
    putTerm lam
    putWord16 $ fromIntegral (length args)
    forM_ args putTerm
  Pi (Name name) inTy outTy -> do
    putWord8 5
    putString name
    putTerm inTy
    putTerm outTy
  Let (Name name) def defTy body -> do
    putWord8 6
    putString name
    putTerm def
    putTerm defTy
    putTerm body
  U1 -> putWord8 7
  U0 -> putWord8 8
  Code ty -> do
    putWord8 9
    putTerm ty
  Quote e -> do
    putWord8 10
    putTerm e
  Splice e -> do
    putWord8 11
    putTerm e
  Hole -> putWord8 12
  MkProd ty fields -> do
    putWord8 13
    putTerm ty
    putWord16 $ fromIntegral (length fields)
    forM_ fields putTerm

moveToTop :: Edit sig m => Ex -> m Item
moveToTop (Ex state) =
  run (MoveOut Left) state >>= \case
    ex@(Ex (EditorState (Cursor focus path) _ _)) -> case path of
      PTop -> pure $ unFItem focus
      _ -> moveToTop ex

-- moveToItem :: Edit sig m => Ex -> m Ex
-- moveToItem (Ex state) =
--   run (MoveOut Left) state >>= \case
--     ex@(Ex (EditorState (Cursor focus path) _ _)) -> case itemName path undefined of
--       Just _ -> pure ex
--       _ -> moveToItem ex

export :: EditIO sig m => EditorState a -> String -> m ()
export state@(EditorState cursor _ _) fn = do
  program <- moveToTop (Ex state) 
  let bs = runPut $ putItem program
  handle <- LE.sendIO $ openFile fn WriteMode
  LE.sendIO $ LB.hPut handle bs
  LE.sendIO $ hClose handle

render :: Edit sig m => EditorState a -> Elab.ElabState -> Item -> m T.Text
render state elabState item = pure $ renderItem item []
  where
    renderItem :: Item -> [String] -> T.Text
    renderItem item gname = case item of
      TermDef (Name n) ty body -> renderTermDef (GName $ n:gname)
      NamespaceDef (Name n) items -> "\ESC[33;1mnamespace\ESC[39m " <> T.pack n <> indentForced (sitems items (n:gname))
      IndDef (Name n) ty cons -> renderIndDef (GName $ n:gname)
      ProdDef (Name n) _ _ -> renderProdDef (GName $ n:gname)
      EditorFocusDef item side -> case side of
        Left -> "\ESC[32;1m{\ESC[0m" <> renderItem item gname <> "\ESC[32;1m]\ESC[0m"
        Right -> "\ESC[32;1m[\ESC[0m" <> renderItem item gname <> "\ESC[32;1m}\ESC[0m"
      EditorBlankDef -> "\ESC[7m?\ESC[27m"
    renderTermDef :: GName -> T.Text
    renderTermDef gname =
      let Just (C.TermDef _ def) = DM.lookup gname (Elab.globals elabState)
      in "\ESC[33;1mval\ESC[39m " <> T.pack (head $ unGName gname) <> " : <todo> = " <> indent (renderTerm def)
    renderIndDef :: GName -> T.Text
    renderIndDef gname =
      let Just (C.IndDef _ ty (C.IndDefInfo cns)) = DM.lookup gname (Elab.globals elabState)
      in "\ESC[33;1minductive\ESC[39m " <> T.pack (head $ unGName gname) <> " : " <> renderTerm ty <> (indentForced $ scons cns (unGName gname))
    renderProdDef :: GName -> T.Text
    renderProdDef gname =
      let Just (C.ProdDef _ ty fields) = DM.lookup gname (Elab.globals elabState)
      in "\ESC[33;1mproduct\ESC[39m " <> T.pack (head $ unGName gname) <> " : " <> renderTerm ty <> (indentForced $ sfields fields)
    renderTerm :: C.Term -> T.Text
    renderTerm (C.Term (C.Info side) term) = case side of
      Just Left -> "\ESC[32;1m{\ESC[0m" <> go term <> "\ESC[32;1m]\ESC[0m"
      Just Right -> "\ESC[32;1m[\ESC[0m" <> go term <> "\ESC[32;1m}\ESC[0m"
      Nothing -> go term
      where
        go :: C.TermInner -> T.Text
        go term = case term of
          C.Var _ _ (C.VarInfo s) -> T.pack s
          C.GVar _ _ (C.GVarInfo s) -> T.pack $ mconcat $ intersperse "." s
          C.Meta _ _ -> "\ESC[7m?\ESC[27m"
          C.InsertedMeta _ _ _ -> "\ESC[7m?\ESC[27m"
          _ -> error $ show term

    multiline s = length (T.lines s) /= 1
    scons cns gname = case cns of
      [] -> ""
      cn:cns ->
        let Just (C.ConDef _ ty) = DM.lookup (GName $ cn:gname) (Elab.globals elabState)
        in T.pack cn <> " : " <> renderTerm ty <> "\n" <> scons cns gname
    sfields fs = mconcat $ intersperse "\n" $ map renderTerm fs
    sitems is gname = case is of
      [] -> ""
      [i] -> renderItem i gname
      i:is -> renderItem i gname <> "\n" <> sitems is gname

    indent :: T.Text -> T.Text
    indent s =
      if not (multiline s) then
        s
      else
        "\n" <> indentBase s
    indent2 :: T.Text -> T.Text
    indent2 s =
      if not (multiline s) then
        s
      else
        "\n" <> (indentBase . indentBase) s
    indentBase :: T.Text -> T.Text
    indentBase s =
      if not (multiline s) then
        s
      else
        (mconcat $ intersperse "\n" $ map ("  "<>) (T.lines s))
    indentForced :: T.Text -> T.Text
    indentForced s = (if s == "" then "" else "\n") <> (mconcat $ intersperse "\n" $ map ("  "<>) (T.lines s))

insertFocusMarker :: EditorState a -> EditorState a
insertFocusMarker state@(EditorState (Cursor focus path) ft side) = case ft of
  FTItem -> EditorState (Cursor (FItem $ EditorFocusDef (unFItem focus) side) path) ft side
  FTTerm -> EditorState (Cursor (FTerm $ EditorFocus (unFTerm focus) side) path) ft side
  _ -> state

-- Lol just Ctrl+C + Ctrl+V from StackOverflow. `hSetBuffering stdin NoBuffering` doesn't work on Windows.
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

data TermInsertionType
  = TILam
  | TILet
  | TIApp
  | TIPi
  | TICode
  | TIQuote
  | TISplice
  | TIMkProd
  deriving Eq

data Input
  = IQuit
  | IExportFile
  | ILoadFile
  | IThenMoveHardRight (Maybe Input)
  | IThenMoveRight (Maybe Input)
  | IThenMoveLeft (Maybe Input)
  | IInsertTermDef
  | IInsertIndDef
  | IInsertProdDef
  | IInsertNamespaceDef
  | IInsertTerm TermInsertionType
  | IInsertU0
  | IInsertU1
  | IAdd
  | ISetName String
  | IDelete
  deriving Eq

getCommand :: String -> IO Input
getCommand acc = do
  putStr "\ESC[2K"
  putStr "\ESC[1000D"
  putStr (reverse acc)
  hFlush stdout
  c <- getHiddenChar
  case c of
    '\b' ->
      if null acc then
        pure IDelete
      else
        getCommand (tail acc)
    _ -> case parseCommand (c:acc) of
      Just cmd -> pure cmd
      Nothing -> getCommand (c:acc)

split :: String -> String -> Char -> [String]
split s acc d = case s of
  [] -> [acc]
  c:cs ->
    if c == d then
      acc : split cs "" d
    else
      split cs (acc ++ [c]) d

parseCommand :: String -> Maybe Input
parseCommand s = case s of
  "q;" -> Just $ IQuit
  "]" -> Just $ IThenMoveRight Nothing
  "[" -> Just $ IThenMoveLeft Nothing
  " mi;" -> Just $ ILoadFile
  " xe;" -> Just $ IExportFile
  "." -> Just $ IThenMoveHardRight $ Just $ IThenMoveRight $ Just $ IInsertTerm TIApp
  " >-" -> Just $ IThenMoveHardRight $ Just $ IThenMoveRight $ Just $ IThenMoveRight $ Just $ IInsertTerm TIPi
  "#" -> Just $ IThenMoveRight $ Just $ IInsertTerm TIMkProd
  " llarof" -> Just $ IThenMoveRight $ Just $ IInsertTerm TIPi
  " lav" -> Just $ IThenMoveRight $ Just $ IInsertTermDef
  " tel" -> Just $ IThenMoveRight $ Just $ IInsertTerm TILet
  "\\" -> Just $ IThenMoveRight $ Just $ IInsertTerm TILam
  " " -> Just IAdd
  " edoc" -> Just $ IThenMoveRight $ Just $ IInsertTerm TICode
  "<" -> Just $ IThenMoveRight $ Just $ IInsertTerm TIQuote
  "~" -> Just $ IThenMoveRight $ Just $ IInsertTerm TISplice
  " dni" -> Just $ IThenMoveRight $ Just IInsertIndDef
  " sn" -> Just $ IThenMoveRight $ Just IInsertNamespaceDef
  " dorp" -> Just $ IThenMoveRight $ Just IInsertProdDef
  ' ':s -> Just $ IThenMoveRight $ Just (go s)
  _ -> Nothing
  where
    go s = case s of
      "0u" -> IInsertU0
      "1u" -> IInsertU1
      _ -> ISetName (reverse s)

moveRight :: Edit sig m => EditorState a -> m Ex
moveRight state = (\c -> run c state) $ case (edge Right (unPath $ unCursor state), atomic (unFocus $ unCursor state), unSide state) of
  (False, False, Left) -> MoveInLeft
  (False, True, Left) -> MoveRight
  (True, False, Left) -> MoveInLeft
  (True, True, Left) -> MoveRight
  (False, False, Right) -> MoveRight
  (False, True, Right) -> MoveRight
  (True, False, Right) -> MoveOut Right
  (True, True, Right) -> MoveOut Right

moveLeft :: Edit sig m => EditorState a -> m Ex
moveLeft state = (\c -> run c state) $ case (edge Left (unPath $ unCursor state), atomic (unFocus $ unCursor state), unSide state) of
  (False, False, Left) -> MoveLeft
  (False, True, Left) -> MoveLeft
  (True, False, Left) -> MoveOut Left
  (True, True, Left) -> MoveOut Left
  (False, False, Right) -> MoveInRight
  (False, True, Right) -> MoveLeft
  (True, False, Right) -> MoveInRight
  (True, True, Right) -> MoveLeft

handleInput :: EditIO sig m => EditorState a -> Input -> m Ex
handleInput state input = case (input, unFocusType state) of
  (IExportFile, _) -> do
    fn <- LE.sendIO $ getLine
    export state fn
    pure $ Ex state
  (ILoadFile, _) -> do
    fn <- LE.sendIO $ getLine
    handle <- LE.sendIO $ openFile fn ReadMode
    bs' <- LE.sendIO $ LB.hGetContents handle
    let !bs = bs'
    let program = runGet getItem bs
    LE.sendIO $ hClose handle
    pure $ mkEx program PTop Left
  (IThenMoveHardRight input', _) -> do
    (Ex state') <- case input' of
      Just input' -> handleInput state input'
      Nothing -> pure $ Ex state
    run MoveRight state'
  (IThenMoveRight input', _) -> do
    (Ex state') <- case input' of
      Just input' -> handleInput state input'
      Nothing -> pure $ Ex state
    moveRight state'
  (IThenMoveLeft input', _) -> do
    (Ex state') <- case input' of
      Just input' -> handleInput state input'
      Nothing -> pure $ Ex state
    moveLeft state'
  -- ("al", _) -> run (Add Left) state
  (IAdd, _) -> run Add state
  -- ("d", FTTerm) -> run InsertHole state
  (IInsertTerm ti, FTTerm) -> run (InsertTerm ti) state
  (IInsertU0, FTTerm) -> run InsertU0 state
  (IInsertU1, FTTerm) -> run InsertU1 state
  (IInsertNamespaceDef, FTItem) -> run InsertNamespaceDef state
  (IInsertTermDef, FTItem) -> run InsertTermDef state
  (IInsertIndDef, FTItem) -> run InsertIndDef state
  (IInsertProdDef, FTItem) -> run InsertProdDef state
  (ISetName s, FTTerm) -> case split s "" '.' of
    [n] -> run (InsertVar n) state
    ns -> run (InsertGVar $ reverse ns) state 
  (ISetName s, FTName) -> if s == "" then pure $ Ex state else run (SetName s) state
  (ISetName s, FTCon) -> if s == "" then pure $ Ex state else run (InsertCon s) state
  (IDelete, _) -> run Delete state
  _ -> pure $ Ex state

loop :: EditIO sig m => EditorState a -> m ()
loop state = do
  LE.sendIO $ clearScreen
  -- (GName ns) <- SE.get
  -- part <- SE.get @(Maybe ItemPart)
  -- changes <- SE.get @Changes
  -- LE.sendIO $ putStrLn $ show changes
  SE.put @Changes mempty
  -- LE.sendIO $ putStrLn $ show part
  -- LE.sendIO $ putStrLn $ show ns
  item <- moveToTop $ Ex $ insertFocusMarker state
  -- LE.sendIO $ putStrLn $ show item'
  let (_, elabState) = Elab.elabFresh item
  -- LE.sendIO $ putStrLn $ show cTerm
  s <- render state elabState item
  LE.sendIO $ TIO.putStrLn (s <> "\n" <> T.pack (show $ Elab.errors elabState))
  LE.sendIO $ hFlush stdout
  input <- LE.sendIO $ getCommand ""
  if input == IQuit then
    pure ()
  else do
    state <- handleInput state input
    next state
    where
      next :: EditIO sig m => Ex -> m ()
      next (Ex state) = loop state

main :: IO ()
main = do
  setLocaleEncoding utf8
  putStr "\ESC[0m"
  state <-
    runM @IO .
    runState @GName (GName ["main"]) .
    runState @Changes DM.empty .
    runState @(Maybe ItemPart) Nothing $
    loop (EditorState (Cursor (FName "main") (PNamespaceDefName PTop [])) FTName Left)
  pure ()