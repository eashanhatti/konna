{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import qualified Core as C
import qualified Norm as N
import Surface
import Var
import qualified Elaboration as Elab
import Elaboration.Term
import Elaboration.Effect
import Control.Algebra(run)
import Control.Carrier.Reader(runReader)
import Control.Carrier.State.Strict(runState)
import Control.Carrier.Error.Either(runError, ErrorC)
-- import qualified PartialEval as PE
import Control.Monad(forM_)
import Data.Map(toList)
import Data.Either(fromRight)
-- import Text.Pretty.Simple(pShow)
import qualified Data.Text.Lazy.IO as Text
import Data.Binary.Get(runGet)
import Data.ByteString.Lazy(readFile)
import Prelude hiding(readFile)

prog =
  TermAst $ Let
    [ItemAst $ TermDef mempty (NameAst $ UserName "_") (TermAst Hole) (TermAst Hole)]
    (TermAst Hole)

main :: IO ()
main = print $ runElab $ check prog (N.gen N.TypeType1)