{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Surface where

import Numeric.Natural
import {-# SOURCE #-} qualified Core as C
import Data.Map(Map)
import Data.Set(Set)
import Elaboration.Error(Error)
import Data.Bifunctor
import Data.Data(Data, mkConstr, Fixity(Prefix), mkDataType, gfoldl, toConstr, dataTypeOf)
import Data.Typeable

data Ast a where
  FocusedAst :: Direction -> Ast a -> Ast a
  ErrorAst :: [Error] -> Ast a -> Ast a
  TermAst :: Term -> Ast Term
  NameAst :: Name -> Ast Name
  ItemAst :: Item -> Ast Item
  ConstructorAst :: Constructor -> Ast Constructor
  ClauseAst :: Clause -> Ast Clause
  PatternAst :: Pattern -> Ast Pattern
deriving instance Show (Ast a)

instance Data a => Data (Ast a) where
  gfoldl k z = \case
    FocusedAst d ast -> z FocusedAst `k` d `k` ast
    ErrorAst es ast -> z ErrorAst `k` es `k` ast
    TermAst e -> z TermAst `k` e
    NameAst n -> z NameAst `k` n
    ItemAst i -> z ItemAst `k` i
    ConstructorAst c -> z ConstructorAst `k` c
    ClauseAst c -> z ClauseAst `k` c
  toConstr = \case
    FocusedAst _ _ -> con_FocusedAst
    ErrorAst _ _ -> con_ErrorAst
    TermAst _ -> con_TermAst
    NameAst _ -> con_NameAst
    ItemAst _ -> con_ItemAst
    ConstructorAst _ -> con_ConstructorAst
    ClauseAst _ -> con_ClauseAst
  dataTypeOf _ = ty_Ast

con_FocusedAst = mkConstr ty_Ast "FocusedAst" [] Prefix
con_ErrorAst = mkConstr ty_Ast "ErrorAst" [] Prefix
con_TermAst = mkConstr ty_Ast "TermAst" [] Prefix
con_NameAst = mkConstr ty_Ast "NameAst" [] Prefix
con_ItemAst = mkConstr ty_Ast "ItemAst" [] Prefix
con_ConstructorAst = mkConstr ty_Ast "ConstructorAst" [] Prefix
con_ClauseAst = mkConstr ty_Ast "ClauseAst" [] Prefix
ty_Ast = mkDataType "Surface.Ast" [con_FocusedAst, con_ErrorAst, con_TermAst, con_NameAst, con_ItemAst, con_ConstructorAst, con_ClauseAst]

type NameAst = Ast Name
data Name = UserName String | MachineName Natural
  deriving (Show, Eq, Ord, Data)

type TermAst = Ast Term
data Term
  = Var Name
  | Lam [NameAst] TermAst
  | App TermAst [TermAst]
  | Ann TermAst TermAst
  | Pi NameAst TermAst TermAst
  | Arrow TermAst TermAst
  | Let [ItemAst] TermAst
  | U0
  | U1
  | Code TermAst
  | Quote TermAst
  | Splice TermAst
  | Match [ClauseAst]
  | Hole
  deriving (Show, Data)

data ItemPart = Sig | Def
  deriving (Show, Eq, Ord, Data)

type SItemInfo = (Map Natural (Set ItemPart), Map ItemPart (C.Term, TermAst))

type ItemAst = Ast Item
data Item
  = TermDef SItemInfo NameAst TermAst TermAst -- name, sig, def
  | IndDef SItemInfo NameAst TermAst [ConstructorAst] -- name, sig, constructors
  | ProdDef SItemInfo NameAst TermAst NameAst [TermAst] -- name, sig, constructor name, fields
  deriving (Show, Data)

unItem :: ItemAst -> (Item, Item -> ItemAst)
unItem = \case
  FocusedAst d ast -> second (FocusedAst d .) (unItem ast)
  ErrorAst es ast -> second (ErrorAst es .) (unItem ast)
  ItemAst item -> (item, ItemAst)

unName :: ItemAst -> Name
unName (unItem -> (item, _)) = case item of
  TermDef _ (NameAst name) _ _ -> name
  IndDef _ (NameAst name) _ _ -> name
  ProdDef _ (NameAst name) _ _ _ -> name

type ConstructorAst = Ast Constructor
data Constructor = Constructor SItemInfo NameAst TermAst
  deriving (Show, Data)

unConstructor :: ConstructorAst -> (Constructor, Constructor -> ConstructorAst)
unConstructor = \case
  FocusedAst d ast -> second (FocusedAst d .) (unConstructor ast)
  ErrorAst es ast -> second (ErrorAst es .) (unConstructor ast)
  ConstructorAst constr -> (constr, ConstructorAst)

type ClauseAst = Ast Clause
data Clause = Clause PatternAst TermAst
  deriving (Show, Data)

type PatternAst = Ast Pattern
data Pattern
  = BindingPat NameAst
  | ConPat NameAst [PatternAst]
  | AppPat [PatternAst]
  deriving (Show, Data)

data Direction = Left | Right
  deriving (Show, Data)