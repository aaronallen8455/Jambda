{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jambda.Types.Newtypes
  ( Sample(..)
  , Cell(..)
  , Freq(..)
  , BPM(..)
  , Sec(..)
  , Octave(..)
  , bpmToString
  ) where

import Foreign.Storable (Storable)
import Text.Printf

newtype Sample = Sample { getSample :: Float } deriving (Show, Eq, Num, Ord, Storable, Enum)

newtype Cell = Cell { getCell :: Double } deriving (Eq, Num, Ord, Fractional, Real, RealFrac)
instance Show Cell where
  show (Cell c) = show c

newtype Freq = Freq { getFreq :: Double } deriving (Show, Eq, Ord, Num)

newtype BPM = BPM { getBPM :: Double } deriving (Show, Eq, Ord, Num, Enum)

bpmToString :: BPM -> String
bpmToString (BPM x) = printf "%.1f" x

newtype Sec = Sec { getSec :: Double } deriving (Show, Eq, Ord, Num)

newtype Octave = Octave { getOctave :: Int } deriving (Show, Eq, Ord, Num)
