{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Jambda.Types.Newtypes
  ( Sample(..)
  , CellValue(..)
  , Freq(..)
  , BPM(..)
  , Sec(..)
  , Octave(..)
  , bpmToString
  ) where

import            Foreign.Storable (Storable)
import            Text.Printf
import            System.Random (Random)

newtype Sample = Sample { getSample :: Float } deriving (Show, Eq, Num, Ord, Storable, Enum)

newtype CellValue = CellValue { getCellValue :: Double } deriving (Eq, Num, Ord, Fractional, Real, RealFrac)
instance Show CellValue where
  show (CellValue c) = show c

newtype Freq = Freq { getFreq :: Double } deriving (Show, Eq, Ord, Num)

newtype BPM = BPM { getBPM :: Double } deriving (Show, Eq, Ord, Num, Enum, Fractional)

bpmToString :: BPM -> String
bpmToString (BPM x) = printf "%.1f" x

newtype Sec = Sec { getSec :: Double } deriving (Show, Eq, Ord, Num)

newtype Octave = Octave { getOctave :: Int } deriving (Show, Eq, Ord, Num, Random)
