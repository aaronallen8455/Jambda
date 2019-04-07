{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.Layer where

import Control.Lens
import Jambda.Types.Newtypes (Cell, Sample, Octave)
import Jambda.Types.Pitch (Pitch)

data Layer =
  Layer
    { _layerSource :: ![Sample]       -- ^ The sound to play
    , _layerBeat :: ![Cell]           -- ^ Infinite list of cells
    , _layerCode :: !String           -- ^ Beat code to create the beat from
    , _layerParsedCode :: ![Cell]     -- ^ The parsed beatcode, finite list
    , _layerCellOffset :: !Cell       -- ^ Initial delay before first note
    , _layerOffsetCode :: !String     -- ^ Code of the offset amount
    , _layerCellPrefix :: !Cell       -- ^ Cell value before next note
    , _layerSourcePrefix :: ![Sample] -- ^ Tail of a partially played source
    , _layerSourceType :: !Pitch       -- ^ The sound representation to use
    }

makeLenses ''Layer
