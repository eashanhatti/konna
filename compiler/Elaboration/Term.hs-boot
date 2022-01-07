module Elaboration.Term where

import Surface
import Elaboration.Effect
import qualified Norm as N
import qualified Core as C

check :: Elab sig m => TermAst -> N.Value -> m (C.Term, TermAst)