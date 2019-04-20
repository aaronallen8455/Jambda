module Jambda.Data.Stream
  ( sineWave
  , silence
  , linearTaper
  ) where

import            Data.Stream.Infinite (Stream(..))
import            qualified  Data.Stream.Infinite as Stream
import            GHC.Float (double2Float, float2Double)

import            Jambda.Types
import            Jambda.Data.Constants (sampleRate)

sineWave :: Freq -> Double -> Stream Sample
sineWave freq phase = go where
  go = v1 :> v2 :> Stream.zipWith (\a b -> c * a - b) (Stream.tail go) go
  len = 2 * pi * getFreq freq / sampleRate
  c = Sample . double2Float $ 2 * cos len
  v1 = Sample . double2Float $ sin $ phase * len
  v2 = Sample . double2Float $ sin $ (phase + 1) * len

silence :: Stream Sample
silence = pure 0

linearTaper :: Double -> Stream Sample -> [Sample]
linearTaper secs = zipWith (*) [1, 1 - step .. 0] . Stream.takeWhile (const True) where
  step = Sample . double2Float $ 1 / secs / sampleRate

