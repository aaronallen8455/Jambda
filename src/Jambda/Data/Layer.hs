{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
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

import            Control.Lens hiding ((:>))
import            Control.Monad (guard)
import            Data.List.NonEmpty (NonEmpty(..))
import            Data.Stream.Infinite (Stream(..))
import qualified  Data.Stream.Infinite as Stream
import qualified  Data.Stream.Infinite as Stream

import            Jambda.Data.Constants (taperLength)
import            Jambda.Data.Conversions (numSamplesForCellValue, numSamplesToCellValue)
import            Jambda.Data.Parsers (parseBeat, parseCell, parseOffset, parsePitch)
import            Jambda.Data.Stream (dovetail, linearTaper, silence, sineWave)
import            Jambda.Types

-- | Create a new layer with the given Pitch using defaults
-- for all other fields
newLayer :: Pitch -> Layer
newLayer pitch = Layer
  { _layerSource = source
  , _layerBeat = pure ( Cell 1 Nothing )
  , _layerCode = "1"
  , _layerParsedCode = [ Cell 1 Nothing ]
  , _layerCellOffset = 0
  , _layerOffsetCode = "0"
  , _layerCellPrefix = 0
  , _layerSourcePrefix = []
  , _layerSourceType = pitch
  }
    where
      freq = pitchToFreq pitch
      source = linearTaper taperLength $ sineWave freq 0

-- | Progress a layer by the given number of samples
-- returning the resulting samples and the modified layer.
readChunk :: Int -> BPM -> Layer -> (Layer, [Sample])
readChunk bufferSize bpm layer@Layer{..}
  | prefixValue >= cellToTake =
    ( layer & layerSourcePrefix %~ (drop bufferSize)
            & layerCellPrefix   -~ cellToTake
    , Stream.take bufferSize $ _layerSourcePrefix `Stream.prepend` silence
    )
  | otherwise = ( remLayer, take bufferSize $ samples )
  where
    cellToTake  = numSamplesToCellValue bpm $ fromIntegral bufferSize
    prefixValue = layer^.layerCellPrefix

    (numPrefixSamples, remPrefixCell) = numSamplesForCellValue bpm prefixValue
    prefixSamples =
      Stream.take numPrefixSamples $ _layerSourcePrefix `Stream.prepend` silence
    (remLayer, newSamples) =
      getSamples bpm
                 ( layer & layerBeat . ix 0 %~ fmap ( + remPrefixCell ) )
                 ( bufferSize - numPrefixSamples )
                 ( drop numPrefixSamples _layerSourcePrefix )
    samples = prefixSamples ++ newSamples

-- | Pull the specified number of samples from a layer.
-- returns the samples and the modified layer.
getSamples :: BPM -> Layer -> Int -> [Sample] -> (Layer, [Sample])
getSamples bpm layer nsamps prevSource
  | nsamps <= wholeCellSamps = (newLayer, take nsamps source)
  | otherwise = _2 %~ ( take wholeCellSamps source ++ )
              $ getSamples bpm
                           ( layer & layerBeat . ix 0 %~ fmap ( + leftover ) )
                           ( nsamps - wholeCellSamps )
                           ( drop wholeCellSamps source )
  where
    ( c :> cells ) = layer^.layerBeat
    source = linearTaper taperLength $
      maybe ( dovetail ( pitchToFreq $ layer^.layerSourceType ) $ prevSource )
            id
            ( dovetail <$> ( pitchToFreq <$> c^.cellSource )
                       <*> Just prevSource
            )
    ( wholeCellSamps, leftover ) = numSamplesForCellValue bpm ( c^.cellValue )
    diff = wholeCellSamps - nsamps
    newCellPrefix = c^.cellValue
                  - numSamplesToCellValue bpm ( fromIntegral nsamps )
                  + leftover
    newLayer = layer & layerBeat         .~ cells
                     & layerCellPrefix   .~ newCellPrefix
                     & layerSourcePrefix .~ (drop nsamps source)

-- | Change the beatcode of a Layer
modifyBeat :: String -> Layer -> Maybe Layer
modifyBeat beatCode layer = do
  cells <- parseBeat beatCode
  guard $ length cells > 0
  pure $ layer & layerBeat       .~ Stream.cycle cells
               & layerCode       .~ beatCode
               & layerParsedCode .~ cells

-- | Change the offset of the layer
modifyOffset :: String -> Layer -> Maybe Layer
modifyOffset offsetCode layer = do
  offset <- parseOffset offsetCode
  pure $ layer & layerCellOffset .~ offset
               & layerOffsetCode .~ offsetCode

-- | Fast-forward a layer to the current time position
syncLayer :: CellValue -> Layer -> Layer
syncLayer elapsedCells layer
  | remainingElapsed <= 0 =
      layer & layerCellPrefix .~ abs remainingElapsed
  | otherwise =
      layer & layerBeat       .~ newCells
            & layerCellPrefix .~ cellPrefix
  where
    remainingElapsed       = elapsedCells - layer^.layerCellOffset
    cycleSize              = sum $ layer^.layerParsedCode^..traverse.cellValue
    elapsedCycles          = remainingElapsed / cycleSize
    wholeCycles            = fromIntegral $ truncate elapsedCycles
    cellsToDrop            = remainingElapsed - wholeCycles * cycleSize
    cellCycle              = Stream.cycle $ layer^.layerParsedCode
    (cellPrefix, newCells) = dropCells cellsToDrop cellCycle
    dropCells !dc ( c :> cs )
      | c^.cellValue >= dc = ( c^.cellValue - dc, cs )
      | otherwise = dropCells ( dc - c^.cellValue ) cs

-- | Change the sound source (Pitch) of the layer
modifySource :: String -> Layer -> Maybe Layer
modifySource noteStr layer = do
  pitch <- parsePitch noteStr
  let freq = pitchToFreq pitch
      wave = sineWave freq 0
      newSource = linearTaper taperLength wave

  pure $ layer & layerSource     .~ newSource
               & layerSourceType .~ pitch

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat         .~ ( Stream.cycle $ layer^.layerParsedCode )
        & layerCellPrefix   .~ ( layer^.layerCellOffset )
        & layerSourcePrefix .~ []

