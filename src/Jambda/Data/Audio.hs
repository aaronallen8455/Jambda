{-# LANGUAGE GADTs #-}
module Jambda.Data.Audio
  ( audioCallback
  , aggregateChunks
  ) where

import Data.List (transpose)
import qualified  Data.IntMap as Map
import Data.IORef
import qualified Data.Vector.Storable.Mutable as MV

import qualified SDL

import Control.Lens

import Jambda.Types
import Jambda.Data.Parsers (parseBeat, parseCell)
import Jambda.Data.Layer (newLayer, readChunk)
import Jambda.Data.Conversions (numSamplesToCells)
import Jambda.Data.Constants (sampleRate)
import Jambda.Data.Stream (linearTaper, silence, sineWave)

audioCallback :: Semaphore
              -> IORef ( Map.IntMap Layer )
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
      chunkMap = readChunk numSamples bpm <$> layers :: Map.IntMap (Layer, [Sample])
      samples = map ( snd . snd ) $ Map.toList chunkMap :: [[Sample]]
      newLayers = fst <$> chunkMap :: Map.IntMap Layer
      combined = map getSample $ aggregateChunks samples :: [Float]
  iforM_ combined $ \i s -> do
    MV.write vec (i * 2) s -- Left channel
    MV.write vec (i * 2 + 1) s -- Right channel

  writeIORef layersRef newLayers
  modifyIORef' elapsedCellsRef (+ numCells)

aggregateChunks :: [[Sample]] -> [Sample]
aggregateChunks = map sum . transpose


