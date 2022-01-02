module Elaboration.Error where

import {-# SOURCE #-} qualified Surface as S
import {-# SOURCE #-} qualified Unification as U

data Error
  = UnboundVar S.Name
  | MismatchVarType [[U.Error]]
  | TooManyParams Int Int -- exp, giv
  | UnifyError U.Error
  deriving Show