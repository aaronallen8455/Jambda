{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jambda.Types.Layer where

import            Control.Lens hiding ((:>))
import            Data.List.NonEmpty (NonEmpty)
import            Data.Stream.Infinite (Stream(..))
import qualified  Data.Stream.Infinite as Stream

import            Jambda.Types.Newtypes (Cell, Sample, Octave)
import            Jambda.Types.Pitch (Pitch)

data Layer =
  Layer
    { _layerSource       :: ![Sample]          -- ^ The sound to play
    , _layerBeat         :: !(Stream Cell)     -- ^ Infinite list of cells
    , _layerCode         :: !String            -- ^ Beat code to create the beat from
    , _layerParsedCode   :: !(NonEmpty Cell)   -- ^ The parsed beatcode, finite list
    , _layerCellOffset   :: !Cell              -- ^ Initial delay before first note
    , _layerOffsetCode   :: !String            -- ^ Code of the offset amount
    , _layerCellPrefix   :: !Cell              -- ^ Cell value before next note
    , _layerSourcePrefix :: ![Sample]          -- ^ Tail of a partially played source
    , _layerSourceType   :: !Pitch             -- ^ The sound representation to use
    }

makeLenses ''Layer

{- Lens related instances for Stream -}

type instance IxValue (Stream a) = a

type instance Index (Stream a) = Int

instance Ixed (Stream a) where
  ix k f xs0 | k < 0     = pure xs0
             | otherwise = go xs0 k where
    go (a :> as) 0 = f a <&> (:> as)
    go (a :> as) i = (a :>) <$> (go as $! i - 1)
  {-# INLINE ix #-}
