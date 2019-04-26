module Jambda.Data.Stream
  ( sineWave
  , silence
  , linearTaper
  , dovetail
  ) where

import            Data.Stream.Infinite (Stream(..))
import            qualified  Data.Stream.Infinite as Stream
import            GHC.Float (double2Float, float2Double)

import            Jambda.Types
import            Jambda.Data.Constants (sampleRate)

-- | Produce an efficient sine wave at the desired
-- frequency and phase. Phase is the number of samples
-- by which to offset the stream.
sineWave :: Freq -> Double -> Stream Sample
sineWave freq phase = go where
  go = v1 :> v2 :> Stream.zipWith ( \a b -> c * a - b )
                                  ( Stream.tail go )
                                  go
  len = 2 * pi * getFreq freq / sampleRate
  c = Sample . double2Float $ 2 * cos len
  v1 = Sample . double2Float $ sin $ phase * len
  v2 = Sample . double2Float $ sin $ (phase + 1) * len

silence :: Stream Sample
silence = pure 0

-- | Applies a linear fade out to a stream over the
-- specified number of seconds.
linearTaper :: Double -> Stream Sample -> [Sample]
linearTaper secs = zipWith (*) [1, 1 - step .. 0]
                 . Stream.takeWhile (const True)
  where
    step = Sample . double2Float $ 1 / secs / sampleRate

-- | Produces a new sinewave at the specified frequency which
-- is phased so as to match the contour and current value
-- of the preceding stream.
dovetail :: Freq -> [Sample] -> Stream Sample
dovetail freq ( s1:s2:_ ) = sineWave freq phase
  where
    asc = s2 >= s1
    quadrant1 = asc && s1 >= 0
    quadrant2 = not asc && s1 > 0
    quadrant3 = not asc && s1 <= 0
    sampPerLen = sampleRate / getFreq freq
    x = asin . float2Double $ getSample s1
    phase | quadrant1 = x / ( 2 * pi ) * sampPerLen
          | quadrant2 = ( pi - x ) / ( 2 * pi ) * sampPerLen
          | quadrant3 = ( -pi - x ) / ( 2 * pi ) * sampPerLen
          | otherwise = x / ( 2 * pi ) * sampPerLen

dovetail freq ( s1:_ ) = sineWave freq phase
  where
    sampPerLen = sampleRate / getFreq freq
    x = asin . float2Double $ getSample s1
    phase = x / ( 2 * pi ) * sampPerLen

dovetail freq [] = sineWave freq 0
