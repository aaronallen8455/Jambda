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
import Jambda.Data.Layer (readChunk)

audioCallback :: Semaphore
              -> IORef ( Map.IntMap Layer )
              -> IORef BPM
              -> IORef Double
              -> IORef Vol
              -> SDL.AudioFormat actualSampleType
              -> MV.IOVector actualSampleType
              -> IO ()
audioCallback semaphore layersRef bpmRef elapsedSamplesRef volumeRef SDL.FloatingLEAudio vec = do
  waitSemaphore semaphore

  layers <- readIORef layersRef
  bpm <- readIORef bpmRef
  ( Vol vol ) <- readIORef volumeRef

  let numSamples = MV.length vec `div` 2                   :: Int
      chunkMap   = readChunk numSamples bpm <$> layers     :: Map.IntMap (Layer, [Sample])
      samples    = map ( snd . snd ) $ Map.toList chunkMap :: [[Sample]]
      newLayers  = fst <$> chunkMap                        :: Map.IntMap Layer
      combined   = map ( ( * ( vol / 10 ) ) . getSample )
                 $ aggregateChunks samples

  iforM_ combined $ \i s -> do
    MV.write vec ( i * 2 ) s     -- Left channel
    MV.write vec ( i * 2 + 1 ) s -- Right channel

  writeIORef layersRef newLayers
  modifyIORef' elapsedSamplesRef ( + fromIntegral numSamples )

audioCallback _ _ _ _ _ fmt _ =
  error $ "Unsupported sample encoding: " <> show fmt

aggregateChunks :: [[Sample]] -> [Sample]
aggregateChunks = map sum . transpose


