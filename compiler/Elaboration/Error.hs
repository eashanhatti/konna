{-# LANGUAGE DeriveDataTypeable #-}

module Elaboration.Error where

import {-# SOURCE #-} qualified Surface as S
import {-# SOURCE #-} qualified Unification as U
import qualified Norm as N
import Data.Data(Data)

data Error
  = UnboundVar S.Name
  | MismatchVarType [[U.Error]]
  | AmbiguousVar [N.Value]
  | ParamNum Int Int -- exp, giv
  | ArgNum Int Int -- exp, giv
  | UnifyError U.Error
  | Todo
  deriving (Show, Data)