{-# LANGUAGE RecordWildCards #-}
module Jambda.Data.Layer
  ( newLayer
  , readChunk
  , getSamples
  , modifyBeat
  , modifyOffset
  , modifySource
  , syncLayer
  , resetLayer
  ) where

import Control.Lens
import Control.Monad (guard)

import Jambda.Types
import Jambda.Data.Stream (linearTaper, silence, sineWave)
import Jambda.Data.Conversions (numSamplesToCells, numSamplesForCell)
import Jambda.Data.Parsers (parseBeat, parseCell, parsePitch)

-- | Create a new layer with the given Pitch using defaults
-- for all other fields
newLayer :: Pitch -> Layer
newLayer pitch = Layer
  { _layerSource = source
  , _layerBeat = repeat 1
  , _layerCode = "1"
  , _layerParsedCode = [1]
  , _layerCellOffset = 0
  , _layerOffsetCode = "0"
  , _layerCellPrefix = 0
  , _layerSourcePrefix = []
  , _layerSourceType = pitch
  }
    where
      freq = pitchToFreq pitch
      source = linearTaper 0.2 $ sineWave freq 0

-- | Progress a layer by the given number of samples
-- returning the resulting samples and the modified layer.
readChunk :: Int -> BPM -> Layer -> (Layer, [Sample])
readChunk bufferSize bpm layer@Layer{..}
  | prefixValue >= cellToTake =
    ( layer & layerSourcePrefix %~ (drop bufferSize)
            & layerCellPrefix   -~ cellToTake
    , take bufferSize $ _layerSourcePrefix ++ silence
   )
  | otherwise = ( remLayer, take bufferSize $ samples)
  where
    cellToTake  = numSamplesToCells bpm $ fromIntegral bufferSize
    prefixValue = layer^.layerCellPrefix

    (numPrefixSamples, remPrefixCell) = numSamplesForCell bpm prefixValue
    prefixSamples = take numPrefixSamples $ _layerSourcePrefix ++ silence
    (remLayer, newSamples) = getSamples bpm
                                        layer
                                        ( _layerBeat & ix 0 +~ remPrefixCell )
                                        (bufferSize - numPrefixSamples)
    samples = prefixSamples ++ newSamples

-- | Pull the specified number of samples from a layer.
-- returns the samples and the modified layer.
getSamples :: BPM -> Layer -> [Cell] -> Int -> (Layer, [Sample])
getSamples _ layer [] _ = (layer, [])
getSamples bpm layer (c:cells) nsamps
  | nsamps <= wholeCellSamps = (newLayer, take nsamps source)
  | otherwise = _2 %~ ( take wholeCellSamps source ++ )
              $ getSamples bpm
                           layer
                           ( cells & ix 0 +~ leftover )
                           ( nsamps - wholeCellSamps )
  where
    source = layer^.layerSource
    ( wholeCellSamps, leftover ) = numSamplesForCell bpm c
    diff = wholeCellSamps - nsamps
    newCellPrefix = c - numSamplesToCells bpm (fromIntegral nsamps) + leftover
    newLayer = layer & layerBeat         .~ cells
                     & layerCellPrefix   .~ newCellPrefix
                     & layerSourcePrefix .~ (drop nsamps source)

-- | Change the beatcode of a Layer
modifyBeat :: Cell -> String -> Layer -> Maybe Layer
modifyBeat elapsedCells beatCode layer = do
  cells <- parseBeat beatCode
  guard $ length cells > 0
  pure . syncLayer elapsedCells $ layer & layerBeat       .~ cycle cells
                                        & layerCode       .~ beatCode
                                        & layerParsedCode .~ cells

-- | Change the offset of the layer
modifyOffset :: Cell -> String -> Layer -> Maybe Layer
modifyOffset elapsedCells offsetCode layer = do
  offset <- parseCell offsetCode
  pure $ syncLayer elapsedCells $ layer & layerCellOffset .~ offset
                                        & layerOffsetCode .~ offsetCode

-- | Fast-forward a layer to the current time position
syncLayer :: Cell -> Layer -> Layer
syncLayer elapsedCells layer
  | remainingElapsed <= 0 =
      layer & layerCellPrefix .~ abs remainingElapsed
  | otherwise =
      layer & layerBeat       .~ newCells
            & layerCellPrefix .~ cellPrefix
  where
    remainingElapsed       = elapsedCells - layer^.layerCellOffset
    cycleSize              = sum $ layer^.layerParsedCode
    elapsedCycles          = remainingElapsed / cycleSize
    wholeCycles            = fromIntegral $ truncate elapsedCycles
    cellsToDrop            = remainingElapsed - wholeCycles * cycleSize
    cellCycle              = cycle $ layer^.layerParsedCode
    (cellPrefix, newCells) = dropCells cellsToDrop cellCycle
    dropCells dc (c:cs)
      | c >= dc = (c - dc, cs)
      | otherwise = dropCells (dc - c) cs

-- | Change the sound source (Pitch) of the layer
modifySource :: String -> Layer -> Maybe Layer
modifySource noteStr layer = do
  pitch <- parsePitch noteStr
  let freq = pitchToFreq pitch
      wave = sineWave freq 0
      newSource = linearTaper 0.2 wave

  pure $ layer & layerSource     .~ newSource
               & layerSourceType .~ pitch

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat         .~ ( cycle $ layer^.layerParsedCode )
        & layerCellPrefix   .~ ( layer^.layerCellOffset )
        & layerSourcePrefix .~ []

