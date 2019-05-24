module Jambda.Types.Pitch
  ( Pitch(..)
  , Note(..)
  , pitchStr
  , pitchToFreq
  ) where

import            Data.Bifunctor
import            System.Random

import Jambda.Types.Newtypes (Freq(..), Octave(..))

data Pitch = Pitch Note Octave
  deriving (Eq, Ord)

instance Random Pitch where
  randomR (Pitch n1 o1, Pitch n2 o2) g =
    let ( octave, g' ) = randomR (o1, o2) g
        ( pitch, g'' ) = first toEnum $ randomR ( fromEnum n1, fromEnum n2 ) g'
     in ( Pitch pitch octave, g'' )
  random = randomR ( Pitch ANat 3, Pitch AFlat 5 )

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
  deriving (Bounded, Enum, Eq, Ord)

pitchStr :: Pitch -> String
pitchStr (Pitch n o) = noteStr n ++ show ( getOctave o )

noteStr :: Note -> String
noteStr ANat  = "A"
noteStr BFlat = "Bb"
noteStr BNat  = "B"
noteStr CNat  = "C"
noteStr DFlat = "Db"
noteStr DNat  = "D"
noteStr EFlat = "Eb"
noteStr ENat  = "E"
noteStr FNat  = "F"
noteStr GFlat = "Gb"
noteStr GNat  = "G"
noteStr AFlat = "Ab"

pitchToFreq :: Pitch -> Freq
pitchToFreq ( Pitch note octave ) = Freq $
    440 * 2 ** ( ( noteIndex + oct * 12 - 48 ) / 12 )
  where
    noteIndex = fromIntegral $ fromEnum note
    oct = fromIntegral $ getOctave octave
