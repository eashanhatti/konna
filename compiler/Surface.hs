module Surface where

import Data.Map(Map)
import Data.Set(Set)

data Span = Span
  deriving (Show, Eq)

data Name = Name { unName :: String }
  deriving (Show, Eq, Ord)

data GName = GName { unGName :: [String] }
  deriving (Show, Eq, Ord)

data Direction = Left | Right
  deriving (Eq, Show)

-- data ItemAttrib = Opaque | Private

data Item
  = NamespaceDef Name [Item]
  | TermDef Name Term Term -- name, dec, def
  | IndDef Name Term [(Name, Term)] -- name, dec, constructors
  | ProdDef Name Term [Term]
  | EditorBlankDef
  | EditorFocusDef Item Direction
  deriving (Show, Eq)

data ItemPart = Dec | Def
  deriving (Eq, Ord, Show)

data Term
  = Var Name
  | GVar GName
  | Lam [Name] Term
  | App Term [Term]
  | Ann Term Term
  | Pi Name Term Term
  | Let Name Term Term Term
  | U0
  | U1
  | Code Term
  | Quote Term
  | Splice Term
  | MkProd Term [Term]
  | Hole
  | EditorBlank
  | EditorFocus Term Direction
  deriving (Show, Eq)