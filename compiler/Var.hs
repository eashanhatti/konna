{-# LANGUAGE DeriveDataTypeable #-}

module Var where

import Data.Data(Data)

newtype Index = Index { unIndex :: Int } deriving (Eq, Ord, Data)

instance Show Index where
  show (Index ix) = "index" ++ show ix

incIndex ix = Index $ unIndex ix + 1

newtype Global = Global { unGlobal :: Int } deriving (Eq, Ord, Data)

instance Show Global where
  show (Global gl) = "global" ++ show gl

newtype Level = Level { unLevel :: Int } deriving (Eq, Ord, Data)

instance Show Level where
  show (Level lv) = "level" ++ show lv

incLevel lv = Level $ unLevel lv + 1

incLevelN :: Int -> Level -> Level
incLevelN n = case n of
  0 -> id
  n -> incLevel . (incLevelN (n - 1))

newtype Id = Id { unId :: Int } deriving (Eq, Ord, Data)

instance Show Id where
  show (Id nid) = "id" ++ show nid