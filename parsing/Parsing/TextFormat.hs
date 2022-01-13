{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Parsing.TextFormat where

import Text.Megaparsec hiding(Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.Megaparsec.Pos(SourcePos)
import Control.Monad.Combinators
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char(isAlphaNum)
import Surface
import Data.Text(Text)
import Data.Void(Void)
import Control.Monad(forM_)
import Control.Monad.State as SM
import Debug.Trace
import Data.Text(stripPrefix)
import qualified Data.Text as Text
import Prelude hiding(lex)

data Token = TLet | TId { unId :: String } | TArrow1 | TArrow0 | TLam
           | TOParen | TCParen | TVal | TDatatype | TIn | THash
           | TSplice | TOQuote | TCQuote | TColon | TOCurly | TDot
           | TCCurly | THole | TSemi | TCode | TU1 | TU0 | TEq
  deriving (Eq, Ord, Show)

instance VisualStream [Token] where
  showTokens _ = show

instance TraversableStream [Token] where
  reachOffset _ s = (Nothing, s)

type Parser = Parsec Void [Token]

name :: Parser NameAst
name = do
  n <- satisfy isId
  pure $ NameAst $ UserName $ unId n

var :: Parser TermAst
var = do
  n <- satisfy isId
  pure $ TermAst $ Var $ UserName $ unId n

isId = \case
  TId _ -> True
  _ -> False

piType :: Parser TermAst
piType = do
  single TOParen
  n <- name
  single TColon
  ty <- prec0
  single TCParen; single TArrow1
  ty' <- prec0
  pure . TermAst $ Pi n ty ty'

lam :: Parser TermAst
lam = do
  single TLam
  ns <- some name
  single TDot
  e <- prec0
  pure . TermAst $ Lam ns e

app :: Parser TermAst
app = do
  l <- prec1;
  es <- some prec1
  pure . TermAst $ App l es

arrType :: Parser TermAst
arrType = do
  -- single THash
  ty <- prec1
  single TArrow0
  ty' <- prec1
  pure . TermAst $ Arrow ty ty'

val :: Parser ItemAst
val = do
  single TVal
  n <- name
  single TColon
  ty <- prec0
  single TEq
  e <- prec0
  pure . ItemAst $ TermDef (mempty, mempty) n ty e

datatype :: Parser ItemAst
datatype = do
  single TDatatype
  n <- name
  single TColon
  ty <- prec0
  single TOCurly
  cs <- some $ con >>= \c -> single TSemi >> pure c
  single TCCurly
  pure . ItemAst $ IndDef (mempty, mempty) n ty cs

con :: Parser ConstructorAst
con = do
  n <- name
  single TColon
  ty <- prec0
  pure . ConstructorAst $ Constructor (mempty, mempty) n ty

item = try val <|> datatype

letB :: Parser TermAst
letB = do
  single TLet; single TOCurly
  is <- many $ item >>= \i -> single TSemi >> pure i
  single TCCurly; single TIn; single TOCurly
  e <- prec0
  single TCCurly
  pure . TermAst $ Let is e

u0 :: Parser TermAst
u0 = single TU0 >> (pure $ TermAst $ U0)
u1 :: Parser TermAst
u1 = single TU1 >> (pure $ TermAst $ U1)

codeType :: Parser TermAst
codeType = do
  single TCode
  ty <- prec1
  pure . TermAst $ Code ty

splice :: Parser TermAst
splice = do
  single TSplice
  e <- prec1
  pure . TermAst $ Splice e

quote :: Parser TermAst
quote = do
  single TOQuote
  e <- prec0
  single TCQuote
  pure . TermAst $ Quote e

hole :: Parser TermAst
hole = single THole >> (pure $ TermAst Hole)

parens :: Parser TermAst -> Parser TermAst
parens t = do
  single TOParen
  e <- t
  single TCParen
  pure e

prec0 = try arrType <|> try app <|> prec1
prec1 = try (parens prec0) <|> try piType <|> try letB <|> try u0 <|> try u1 <|> try hole <|> try lam <|> try codeType <|> try splice <|> try quote <|> try hole <|> var

parse ts = snd $ runParser' (prec0 >>= \e -> eof >> pure e) (State ts 0 (PosState ts 0 (SourcePos "<filename>" pos1 pos1) pos1 "") [])

lex :: Text -> SM.State String [Token]
lex s = case s of
  ((==0) . Text.length -> True) -> pure []
  (stripPrefix " " -> Just s) -> lex s
  (stripPrefix "\n" -> Just s) -> lex s
  _ -> do
    let
      m = case s of
        (stripPrefix "let" -> Just s) -> Just (TLet, s)
        (stripPrefix "->" -> Just s) -> Just (TArrow1, s)
        (stripPrefix "~>" -> Just s) -> Just (TArrow0, s)
        (stripPrefix "\\" -> Just s) -> Just (TLam, s)
        (stripPrefix "(" -> Just s) -> Just (TOParen, s)
        (stripPrefix ")" -> Just s) -> Just (TCParen, s)
        (stripPrefix "val" -> Just s) -> Just (TVal, s)
        (stripPrefix "datatype" -> Just s) -> Just (TDatatype, s)
        (stripPrefix "in" -> Just s) -> Just (TIn, s)
        (stripPrefix "#" -> Just s) -> Just (THash, s)
        (stripPrefix "~" -> Just s) -> Just (TSplice, s)
        (stripPrefix "<" -> Just s) -> Just (TOQuote, s)
        (stripPrefix ">" -> Just s) -> Just (TCQuote, s)
        (stripPrefix ":" -> Just s) -> Just (TColon, s)
        (stripPrefix "{" -> Just s) -> Just (TOCurly, s)
        (stripPrefix "}" -> Just s) -> Just (TCCurly, s)
        (stripPrefix "." -> Just s) -> Just (TDot, s)
        (stripPrefix "_" -> Just s) -> Just (THole, s)
        (stripPrefix ";" -> Just s) -> Just (TSemi, s)
        (stripPrefix "Code" -> Just s) -> Just (TCode, s)
        (stripPrefix "U1" -> Just s) -> Just (TU1, s)
        (stripPrefix "U0" -> Just s) -> Just (TU0, s)
        (stripPrefix "=" -> Just s) -> Just (TEq, s)
        _ -> Nothing
    case m of
      Just (t, s) -> (t:) <$> lex s
      Nothing -> lexVar s
  where
    lexVar s = case Text.uncons s of
      Just (c, s) ->
        if isAlphaNum c then do
          n <- get
          put (n++[c])
          lexVar s
        else do
          n <- get
          put ""
          (TId n :) <$> lex (Text.cons c s)
      Nothing -> pure []