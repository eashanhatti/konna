{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Surface where

import Numeric.Natural
import {-# SOURCE #-} qualified Core as C
import Data.Map(Map)
import Data.Set(Set)
import Elaboration.Error(Error)
import Data.Bifunctor

data Ast a where
  FocusedAst :: Direction -> Ast a -> Ast a
  ErrorAst :: [Error] -> Ast a -> Ast a
  TermAst :: Term -> Ast Term
  NameAst :: Name -> Ast Name
  ItemAst :: Item -> Ast Item
  ConstructorAst :: Constructor -> Ast Constructor
  ClauseAst :: Clause -> Ast Clause
deriving instance Show (Ast a)

type NameAst = Ast Name
data Name = UserName String | MachineName Natural
  deriving (Show, Eq, Ord)

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
  deriving Show

data ItemPart = Sig | Def
  deriving (Show, Eq, Ord)
-- Bool: Must be rechecked. Starts out False for items that have been changed
-- [Map Name ItemPart]: Dependencies
-- Map ItemPart C.Term: Old parts, if they have not been changed. Stores a dummy `C.Term` if the item never has one
data ItemInfo = ItemInfo Bool (Map Name (Set ItemPart)) (Map ItemPart C.Term)
  deriving Show

type ItemAst = Ast Item
data Item
  = TermDef ItemInfo NameAst TermAst TermAst -- name, sig, def
  | IndDef ItemInfo NameAst TermAst [ConstructorAst] -- name, sig, constructors
  | ProdDef ItemInfo NameAst TermAst NameAst [TermAst] -- name, sig, constructor name, fields
  deriving Show

unOldParts :: Item -> Map ItemPart C.Term
unOldParts = \case
  TermDef (ItemInfo _ _ ps) _ _ _ -> ps
  IndDef (ItemInfo _ _ ps) _ _ _ -> ps
  ProdDef (ItemInfo _ _ ps) _ _ _ _ -> ps

withOldParts :: Map ItemPart C.Term -> Item -> Item
withOldParts ps = \case
  TermDef (ItemInfo b d _) n s e -> TermDef (ItemInfo b d ps) n s e
  IndDef (ItemInfo b d _) n s cs -> IndDef (ItemInfo b d ps) n s cs
  ProdDef (ItemInfo b d _) n s cn fs -> ProdDef (ItemInfo b d ps) n s cn fs

unShouldRecheck :: Item -> Bool
unShouldRecheck = \case
  TermDef (ItemInfo b _ _) _ _ _ -> b
  IndDef (ItemInfo b _ _) _ _ _ -> b
  ProdDef (ItemInfo b _ _) _ _ _ _ -> b

withShouldRecheck :: Bool -> Item -> Item
withShouldRecheck b = \case
  TermDef (ItemInfo _ d ps) n s e -> TermDef (ItemInfo b d ps) n s e
  IndDef (ItemInfo _ d ps) n s cs -> IndDef (ItemInfo b d ps) n s cs
  ProdDef (ItemInfo _ d ps) n s cn fs -> ProdDef (ItemInfo b d ps) n s cn fs

unDependencies :: Item -> Map Name (Set ItemPart)
unDependencies = \case
  TermDef (ItemInfo _ d _) _ _ _ -> d
  IndDef (ItemInfo _ d _) _ _ _ -> d
  ProdDef (ItemInfo _ d _) _ _ _ _ -> d

unName :: Item -> Name
unName = \case
  TermDef _ (NameAst n) _ _ -> n
  IndDef _ (NameAst n) _ _ -> n
  ProdDef _ (NameAst n) _ _ _ -> n

unItemInfo :: Item -> ItemInfo
unItemInfo = \case
  TermDef i _ _ _ -> i
  IndDef i _ _ _ -> i
  ProdDef i _ _ _ _ -> i

unItem :: ItemAst -> (Item, Item -> ItemAst)
unItem = \case
  FocusedAst d ast -> second (FocusedAst d .) (unItem ast)
  ErrorAst es ast -> second (ErrorAst es .) (unItem ast)
  ItemAst item -> (item, ItemAst)

type ConstructorAst = Ast Constructor
data Constructor = Constructor ItemInfo NameAst TermAst
  deriving Show

type ClauseAst = Ast Clause
data Clause = Clause PatternAst TermAst
  deriving Show

type PatternAst = Ast Pattern
data Pattern
  = BindingPat NameAst
  | ConPat NameAst [PatternAst]
  | AppPat [PatternAst]
  deriving Show

data Direction = Left | Right
  deriving Show