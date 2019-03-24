{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jambda.Newtypes
  ( Sample(..)
  , Cell(..)
  , Freq(..)
  , BPM(..)
  , Sec(..)
  ) where

import Foreign.Storable (Storable)

newtype Sample = Sample { getSample :: Float } deriving (Show, Eq, Num, Ord, Storable, Enum)

newtype Cell = Cell { getCell :: Double } deriving (Show, Eq, Num, Ord, Fractional)

newtype Freq = Freq { getFreq :: Double } deriving (Show, Eq, Ord, Num)

newtype BPM = BPM { getBPM :: Double } deriving (Show, Eq, Ord, Num, Enum)

newtype Sec = Sec { getSec :: Double } deriving (Show, Eq, Ord, Num)
