{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}

module Surface where

import Numeric.Natural
import {-# SOURCE #-} qualified Core as C
import Data.Map(Map)
import Elaboration.Error(Error)

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
  | Con TermAst [TermAst]
  | Match [ClauseAst]
  | Hole
  deriving Show

data ItemPart = Sig | Def
  deriving Show
data PrevItem
  = Changed [Name]
  | Unchanged [Name] (Map ItemPart C.Term)
  deriving Show

type ItemAst = Ast Item
data Item
  = TermDef PrevItem NameAst TermAst TermAst -- name, sig, def
  | IndDef PrevItem NameAst TermAst [ConstructorAst] -- name, sig, constructors
  | ProdDef PrevItem NameAst TermAst NameAst [TermAst] -- name, sig, constructor name, fields
  deriving Show

type ConstructorAst = Ast Constructor
data Constructor = Constructor PrevItem NameAst TermAst
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

-- import Data.Map(Map)
-- import Data.Set(Set)
-- import Data.Data(Data)

-- data Name = UnfocusedName String | FocusedName String Direction
--   deriving (Show, Eq, Ord, Data)

-- pattern Name s <- (unName -> s) where
--   Name s = UnfocusedName s

-- unName name = case name of
--   UnfocusedName s -> s
--   FocusedName s _ -> s

-- data GName = UnfocusedGName [String] | FocusedGName [String] Direction
--   deriving (Show, Eq, Ord, Data)

-- pattern GName ns <- (unGName -> ns) where
--   GName ns = UnfocusedGName ns

-- unGName name = case name of
--   UnfocusedGName ns -> ns
--   FocusedGName ns _ -> ns

-- data Constructor = FocusedConstructor Name Term | UnfocusedConstructor Name Term | EditorBlankCon
--   deriving (Show, Eq, Data)

-- pattern Constructor n t <- (unCon -> (n, t)) where
--   Constructor n t = UnfocusedConstructor n t

-- unCon con = case con of
--   UnfocusedConstructor n t -> (n, t)
--   FocusedConstructor n t -> (n, t)

-- data Direction = Left | Right
--   deriving (Eq, Ord, Show, Data)

-- data Item
--   | TermDef Name Term Term -- name, dec, def
--   | IndDef Name Term [Constructor] -- name, dec, constructors
--   | ProdDef Name Term [Term]
--   | EditorBlankDef
--   | EditorFocusDef Item Direction
--   deriving (Show, Eq, Data)

-- data ItemPart = Dec | Def
--   deriving (Eq, Ord, Show)

-- data Pattern
--   = BindingPat Name
--   | ConPat GName [Pattern]
--   | AppPat [Pattern]
--   | EditorFocusPat Pattern Direction
--   deriving (Show, Eq, Data)

-- data Clause = UnfocusedClause Pattern Term | FocusedClause Pattern Term | EditorBlankClause
--   deriving (Show, Eq, Data)

-- pattern Clause p t <- (unClause -> (p, t)) where
--   Clause p t = UnfocusedClause p t

-- unClause clause = case clause of
--   UnfocusedClause p t -> (p, t)
--   FocusedClause p t -> (p, t)