{-# LANGUAGE GADTs #-}

module Surface where

import Numeric.Natural

data AstInner a where
  FocusedAst :: Ast a -> Direction -> AstInner a
  BlankAst :: AstInner a
  TermAst :: Term -> AstInner Term
  NameAst :: Name -> AstInner Name
  ItemAst :: Item -> AstInner Item
  ConstructorAst :: Constructor -> AstInner Constructor
  ClauseAst :: Clause -> AstInner Clause

data Ast a = Ast (AstInner a)

type NameAst = Ast Name
data Name = UserName String | MachineName Natural

type TermAst = Ast Term
data Term
  = Var Name
  | Lam [Name] TermAst
  | App TermAst [TermAst]
  | Ann TermAst TermAst
  | Pi Name TermAst TermAst
  | Let Name TermAst TermAst TermAst
  | U0
  | U1
  | Code TermAst
  | Quote TermAst
  | Splice TermAst
  | Con TermAst [TermAst]
  | Match [ClauseAst]
  | Hole

type ItemAst = Ast Item
data Item
  = TermDef NameAst TermAst TermAst -- name, dec, def
  | IndDef NameAst TermAst [Constructor] -- name, dec, constructors
  | ProdDef NameAst TermAst [TermAst]

type ConstructorAst = Ast Constructor
data Constructor = Constructor NameAst TermAst

type ClauseAst = Ast Clause
data Clause = Clause PatternAst TermAst

type PatternAst = Ast Pattern
data Pattern
  = BindingPat NameAst
  | ConPat NameAst [PatternAst]
  | AppPat [PatternAst]

data Direction = Left | Right

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