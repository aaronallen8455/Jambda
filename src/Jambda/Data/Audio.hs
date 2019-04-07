{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Jambda.Data.Audio
  ( audioCallback
  , aggregateChunks
  , sampleRate
  , sineWave
  , silence
  , linearTaper
  , readChunk
  , pitchToFreq
  ) where

import Data.List (transpose)
import Data.IORef
import qualified Data.Vector.Storable.Mutable as MV
import GHC.Float (double2Float, float2Double)

import qualified SDL

import Control.Lens

import Jambda.Types
import Jambda.Data.Parsers (parseBeat, parseCell)

audioCallback :: Semaphore
              -> IORef [Layer]
              -> IORef BPM
              -> IORef Cell
              -> SDL.AudioFormat actualSampleType
              -> MV.IOVector actualSampleType
              -> IO ()
audioCallback semaphore layersRef bpmRef elapsedCellsRef SDL.FloatingLEAudio vec = do
  waitSemaphore semaphore

  layers <- readIORef layersRef
  bpm <- readIORef bpmRef

  let numSamples = MV.length vec `div` 2
      numCells = numSamplesToCells bpm $ fromIntegral numSamples
      (newLayers, samples) = unzip $ map (readChunk numSamples bpm) layers
      combined = map getSample $ aggregateChunks samples
  iforM_ combined $ \i s -> do
    MV.write vec (i * 2) s -- Left channel
    MV.write vec (i * 2 + 1) s -- Right channel

  writeIORef layersRef newLayers
  modifyIORef' elapsedCellsRef (+ numCells)

testCB _ _ f vec = do
  pure ()

aggregateChunks :: [[Sample]] -> [Sample]
aggregateChunks = map sum . transpose

sampleRate :: Double
sampleRate = 44100

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

-- | Progress a layer by the given number of sampels
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
    cellToTake = numSamplesToCells bpm $ fromIntegral bufferSize
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

-- | Get the number of whole samples from a cell value and also
-- return the cell value corresponding to the leftover fractional sample
numSamplesForCell :: BPM -> Cell -> (Int, Cell)
numSamplesForCell bpm cell = (wholeSamps, remCell) where
  samples = secToNumSamps $ cellToSecs bpm cell
  wholeSamps = floor samples
  remCell = numSamplesToCells bpm $ samples - fromIntegral wholeSamps

numSamplesToCells :: BPM -> Double -> Cell
numSamplesToCells bpm = secToCells bpm . numSampsToSecs

cellToSecs :: BPM -> Cell -> Sec
cellToSecs (BPM bpm) (Cell cell) = Sec $
  1 / (bpm / 60) * cell

secToCells :: BPM -> Sec -> Cell
secToCells (BPM bpm) (Sec sec) = Cell $ bpm / 60 * sec

numSampsToSecs :: Double -> Sec
numSampsToSecs nsamps = Sec $
  nsamps / sampleRate

secToNumSamps :: Sec -> Double
secToNumSamps (Sec sec) = sec * sampleRate
