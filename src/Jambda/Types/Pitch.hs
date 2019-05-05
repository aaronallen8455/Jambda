module Jambda.Types.Pitch
  ( Pitch(..)
  , Note(..)
  , pitchToFreq
  ) where

import Jambda.Types.Newtypes (Freq(..), Octave(..))

data Pitch = Pitch Note Octave
  deriving (Eq, Ord)

instance Show Pitch where
  show (Pitch n o) = show n ++ show ( getOctave o )

data Note
  = ANat
  | BFlat
  | BNat
  | CNat
  | DFlat
  | DNat
  | EFlat
  | ENat
  | FNat
  | GFlat
  | GNat
  | AFlat
  deriving (Enum, Eq, Ord)

instance Show Note where
  show ANat  = "A"
  show BFlat = "Bb"
  show BNat  = "B"
  show CNat  = "C"
  show DFlat = "Db"
  show DNat  = "D"
  show EFlat = "Eb"
  show ENat  = "E"
  show FNat  = "F"
  show GFlat = "Gb"
  show GNat  = "G"
  show AFlat = "Ab"

pitchToFreq :: Pitch -> Freq
pitchToFreq ( Pitch note octave ) = Freq $ 440 * 2 ** ( ( noteIndex + oct * 12 - 48 ) / 12 ) where
  noteIndex = fromIntegral $ fromEnum note
  oct = fromIntegral $ getOctave octave
