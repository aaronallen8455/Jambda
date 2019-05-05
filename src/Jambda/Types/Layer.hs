{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jambda.Types.Layer where

import            Control.Lens hiding ((:>))
import            Data.List.NonEmpty (NonEmpty)
import            Data.Stream.Infinite (Stream(..))

import            Jambda.Types.Newtypes (CellValue, Sample)
import            Jambda.Types.Pitch (Pitch)
import            Jambda.Types.Cell (Cell')

data Layer =
  Layer
    { _layerSource       :: ![Sample]           -- ^ The sound to play
    , _layerBeat         :: !(Stream Cell')     -- ^ Infinite list of cells
    , _layerCode         :: !String             -- ^ Beat code to create the beat from
    , _layerParsedCode   :: !(NonEmpty Cell')   -- ^ The parsed beatcode, finite list
    , _layerCellOffset   :: !CellValue          -- ^ Initial delay before first note
    , _layerOffsetCode   :: !String             -- ^ Code of the offset amount
    , _layerCellPrefix   :: !CellValue          -- ^ Cell value before next note
    , _layerSourcePrefix :: ![Sample]           -- ^ Tail of a partially played source
    , _layerSourceType   :: !Pitch              -- ^ The sound representation to use
    }

makeLenses ''Layer

