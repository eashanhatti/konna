{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Core as C
import qualified Norm as N
import Surface hiding(Left, Right)
import Var
import qualified Elaboration as Elab
import Elaboration.Term
import Elaboration.Effect
import Control.Algebra(run)
import Control.Carrier.Reader(runReader)
import Control.Carrier.State.Strict(runState)
import Control.Monad.State as SM
import Control.Carrier.Error.Either(runError, ErrorC)
-- import qualified PartialEval as PE
import Control.Monad(forM_)
import Data.Map(toList)
import Data.Either(fromRight)
-- import Text.Pretty.Simple(pShow)
import qualified Data.Text.IO as Text
import Data.Binary.Get(runGet)
import Data.ByteString.Lazy(readFile)
import Prelude hiding(readFile, lex)
import Parsing.TextFormat(parse, lex)
import Text.Megaparsec.Error(errorBundlePretty)

main :: IO ()
main = do
  file <- Text.readFile "source.kon"
  let tokens = SM.evalState (lex file) ""
  print tokens
  let prog = parse tokens
  case prog of
    Right prog -> do
      let !prog' = prog
      print prog
      print $ runElab $ check prog' (N.gen N.TypeType0)
    Left e -> putStrLn $ errorBundlePretty e