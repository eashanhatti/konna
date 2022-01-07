module Norm where

import Data.Data(Data)

data Value

type Type = Value

instance Show Value where
instance Eq Value where
instance Ord Value where
instance Data Value where