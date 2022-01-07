{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Core where

import Var
import {-# SOURCE #-} qualified Norm as N
import Control.Monad.Reader(Reader, ask)
import Data.Map(Map)
import Data.Set(Set)
import Data.List(intersperse)
import Numeric.Natural
import Debug.Trace(trace)
import Data.Maybe(isJust)
import Etc

data BinderInfo = Abstract | Concrete
  deriving (Show, Eq, Ord)

-- type annotation
type Type = Term

data Program = Program [Item]

instance Show Program where
  show (Program is) = concat $ intersperse "\n" (map show is)

data Item
  = TermDef Id Type Term
  | IndDef Id Type
  | ProdDef Id Type
  | ConDef Id Type
  | SigDef Id Type
  deriving Eq

instance Show Item where
  show item = case item of
    TermDef nid body ty -> "def " ++ show nid ++ " : " ++ show ty ++ " = " ++ show body
    IndDef nid ty -> "ind " ++ show nid ++ " : " ++ show ty
    ProdDef nid ty -> "prod " ++ show nid ++ " : " ++ show ty
    ConDef nid ty -> "con " ++ show nid ++ " : " ++ show ty
    SigDef nid _ -> "sig " ++ show nid

itemId item = case item of
  TermDef nid _ _ -> nid
  IndDef nid _ -> nid
  ConDef nid _ -> nid
  ProdDef nid _ -> nid
  SigDef nid _ -> nid
data Term = Term
  { unTerm :: TermInner }
  deriving (Eq, Ord)

gen = Term

data TermInner
  = Var Index Type
  | GVar Id Type
  | TypeType0
  | TypeType1
  | FunIntro Term Type
  | FunType Term Term
  | FunElim Term Term
  | QuoteType Term
  | QuoteIntro Term Type
  | QuoteElim Term
  | IndType Id [Term]
  | IndIntro Id [Term] Type
  | IndElim Term [Term]
  | ProdType Id [Term]
  | ProdIntro Type [Term]
  | ProdElim Term Term
  | Letrec [Term] Term
  | Meta Global (Maybe Type)
  | InsertedMeta [BinderInfo] Global (Maybe Type)
  | ElabError
  | Impossible
  deriving (Eq, Ord)

-- getType :: Term -> Term
-- getType (Term _ term) = case term of
--   Var _ ty _ -> ty
--   GVar _ ty _ -> ty
--   TypeType0 -> gen TypeType1
--   TypeType1 -> gen TypeType1
--   FunIntro _ ty _ -> ty
--   FunType inTy _ _ -> getType inTy
--   FunElim (unTerm . getType -> FunType _ outTy _) _ _ -> outTy
--   QuoteType _ -> gen TypeType1
--   QuoteIntro _ ty -> ty
--   QuoteElim (unTerm . getType -> QuoteType ty) -> ty
--   IndType _ _ -> gen TypeType1
--   IndIntro _ _ ty -> ty
--   ProdType _ _ -> gen TypeType0
--   ProdIntro ty _ -> ty
--   Letrec _ body -> getType body
--   Meta _ (Just ty) -> ty
--   Meta _ Nothing -> gen TypeType1
--   InsertedMeta _ _ (Just ty) -> ty
--   InsertedMeta _ _ Nothing -> gen TypeType1
--   ElabError _ -> gen ElabError

instance Show TermInner where
  show term = case term of
    Var ix ty -> "i" ++ show (unIndex ix) -- ++ ":" ++ show ty
    TypeType0 -> "U0"
    TypeType1 -> "U1"
    FunIntro body ty -> "{" ++ show body ++ "}"
    FunType inTy outTy -> show inTy ++ " -> " ++ show outTy
    FunElim lam arg -> "(" ++ show lam ++ " @ " ++ show arg ++ ")"
    QuoteType innerTy -> "Quote " ++ show innerTy
    QuoteIntro inner _ -> "<" ++ show inner ++ ">"
    QuoteElim quote -> "[" ++ show quote ++ "]"
    Letrec defs body -> "letrec " ++ show defs ++ " in " ++ show body
    Meta gl ty ->
      -- if showTys then
        "(?" ++ show (unGlobal gl) ++ " : " ++ show ty ++ ")"
      -- else
      --   "?" ++ show (unGlobal gl)
    InsertedMeta bis gl ty ->
      "(?" ++ show (unGlobal gl) ++ " : " ++ show ty ++ ";" ++ (show $ Prelude.map show bis) ++ ")"
    GVar nid ty -> "g" ++ show (unId nid){- ++ ":" ++ show ty ++ ")"-}
    IndIntro (Id nid) args ty -> "#" ++ show nid ++ "[" ++ (concat $ intersperse ", " $ Prelude.map show args) ++ "]" ++ ":(" ++ show ty ++ ")"
    IndType (Id nid) indices -> "Ind" ++ show nid ++ "[" ++ (concat $ intersperse ", " $ Prelude.map show indices) ++ "]"
    IndElim scr bs -> "case " ++ show scr ++ " of" ++ show bs
    ProdIntro ty fields -> "{" ++ (concat $ intersperse ", " $ Prelude.map show fields) ++ "}" ++ ":" ++ show ty
    ProdType nid indices -> "Prod" ++ show nid ++ "[" ++ (concat $ intersperse ", " $ Prelude.map show indices) ++ "]"
    ElabError -> "<error>"
    Impossible -> "<impossible>"

-- shift :: Set Index -> Term -> Reader Int Term
-- shift bounds = \case
--   Var ix ty ->
--     if ix `member` bounds then
--       pure $ Var ix ty
--     else do
--       amt <- ask
--       pure $ Var (Index $ unIndex ix + amt) ty
--   TypeType -> pure TypeType
--   FunIntro body ty -> FunIntro <$> shift next body <*> shift bounds ty
--   FunType inTy outTy -> FunType <$> shift bounds inTy <*> shift next outTy
--   FunElim lam arg -> FunElim <$> shift bounds lam <*> shift bounds arg
--   Let def defTy body -> Let <$> shift bounds def <*> shift bounds defTy <*> shift next body
--   Meta gl ty -> Meta <$> pure gl <*> shift bounds ty
--   InsertedMeta bis gl ty -> InsertedMeta <$> pure bis <*> pure gl <*> shift bounds ty
--   ElabError -> pure ElabError
--   _ -> error "Unimplemented"
--   where
--     next :: Set Index
--     next = insert (Index 0) $ Data.Set.map (\ix -> Index $ unIndex ix + 1) bounds

instance Show Term where
  show (Term term) = show term