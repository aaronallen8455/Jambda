module Jambda.Data.Stream
  ( sineWave
  , silence
  , linearTaper
  ) where

import GHC.Float (double2Float, float2Double)

import Jambda.Types
import Jambda.Data.Constants (sampleRate)

sineWave :: Freq -> Double -> [Sample]
sineWave freq phase = go where
  go = v1 : v2 : zipWith (\a b -> c * a - b) (tail go) go
  len = 2 * pi * getFreq freq / sampleRate
  c = Sample . double2Float $ 2 * cos len
  v1 = Sample . double2Float $ sin $ phase * len
  v2 = Sample . double2Float $ sin $ (phase + 1) * len

silence :: [Sample]
silence = repeat 0

linearTaper :: Double -> [Sample] -> [Sample]
linearTaper secs = zipWith (*) [1, 1 - step .. 0] where
  step = Sample . double2Float $ 1 / secs / sampleRate

