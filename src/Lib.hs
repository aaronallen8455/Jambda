{-# LANGUAGE RecordWildCards #-}
module Lib
  ( play
  , sineWave
  , linearTaper
  , test
  , testBanana
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad (when, void, forever)
import Data.Function (fix)
import Data.IORef
import Sound.Pulse.Simple
import System.IO (hReady, hSetEcho, stdin)
import GHC.Float (double2Float, float2Double)
import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.List

import Synth.Newtypes (BPM(..), Cell(..), Freq(..), Sample(..), Sec(..))
import Synth.Types

import Debug.Trace

import qualified Sound.Sox.Play as Play
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Basic.Binary as BinSmp
import qualified Sound.Sox.Option.Format as SoxOpt
import System.Exit (ExitCode)
import Data.IORef

buildLazySig :: IORef Bool -> IO (SigSt.T Double)
buildLazySig switch = do
  let source = map (float2Double . getSample) $ sineWave 440 0
      doStuff source = do
          quiet <- readIORef switch
          let (chunk, rest) = splitAt 1024 source
              chunk' | quiet = SigSt.fromList SigSt.defaultChunkSize $ map (*0.3) chunk
                     | otherwise = SigSt.fromList SigSt.defaultChunkSize chunk
          SigSt.append <$> pure chunk' <*> doStuff rest
  list <- doStuff source
  pure list


playL :: SigSt.T Double -> IO ExitCode
playL
  = Play.simple SigSt.hPut SoxOpt.none 44100
  . SigSt.map BinSmp.int16FromDouble

testDis :: IO ()
testDis = do
  switch <- newIORef False
  sig <- buildLazySig switch

  putStrLn "test"

  forkIO . void $ playL sig
  fix $ \rec -> do
    inp <- getLine
    case inp of
      "x" -> do
        modifyIORef switch not
        rec
      _ -> rec

testBanana :: BPM -> [Layer] -> IO ()
testBanana bpm layers = do
  s <- simpleNew Nothing "example" Play Nothing "this is an example application"
         (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing

  runRef <- newIORef True
  bpmRef <- newIORef bpm

  let loop layers = do
        isRunning <- readIORef runRef
        when isRunning $ do
          bpm <- readIORef bpmRef
          let (newLayers, chunks) = unzip $ map (readChunk bpm) layers
          simpleWrite s $ aggregateChunks chunks
          loop newLayers

  forkIO $ loop layers

  (addKeyEvent, fireKey) <- newAddHandler

  let makeNetwork = do
        eKey <- fromAddHandler addKeyEvent
        let eTempo = filterJust $ handleKey <$> eKey
            eStop  = filterE id $ (== "x") <$> eKey

        reactimate $ modifyIORef bpmRef <$> eTempo
        reactimate $ finalizer <$ eStop

      finalizer = do
        writeIORef runRef False
        simpleDrain s
        simpleFree s

  network <- compile makeNetwork
  actuate network

  hSetEcho stdin False
  forever $ getKey >>= fireKey

handleKey :: Enum a => String -> Maybe (a -> a)
handleKey "\ESC[A" = Just succ
handleKey "\ESC[B" = Just pred
handleKey _ = Nothing

test :: IO ()
test = testBanana (BPM 120) [layer, layer2]

play :: BPM -> [Layer] -> IO ()
play bpm layers = do
  s <- simpleNew Nothing "example" Play Nothing "this is an example application"
         (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing

  runRef <- newIORef True
  bpmRef <- newIORef bpm

  let loop layers = do
        isRunning <- readIORef runRef
        when isRunning $ do
          bpm <- readIORef bpmRef
          let (newLayers, chunks) = unzip $ map (readChunk bpm) layers
          simpleWrite s $ aggregateChunks chunks
          loop newLayers

  forkIO $ loop layers

  fix $ \rec -> do
    inp <- getKey
    case inp of
      "x" -> do
        writeIORef runRef False
        simpleDrain s
        simpleFree s
      "\ESC[A" -> do
        modifyIORef bpmRef succ
        rec
      "\ESC[B" -> do
        modifyIORef bpmRef pred
        rec
      _ -> rec

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

layer :: Layer
layer =
  Layer
    { _layerSource = linearTaper 0.2 $ sineWave 440 0
    , _layerBeat = cycle $ [1, 1/3]
    , _layerCellPrefix = 0
    , _layerSourcePrefix = []
    }

layer2 :: Layer
layer2 =
  Layer
    { _layerSource = linearTaper 0.2 $ sineWave 550 0
    , _layerBeat = cycle $ [1, 0.5, 0.5]
    , _layerCellPrefix = 0
    , _layerSourcePrefix = []
    }

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

bufferSize :: Int
bufferSize = 1000

readChunk :: BPM -> Layer -> (Layer, [Sample])
readChunk bpm layer@Layer{..}
  | remLen >= bufferSize =
    (   layerSourcePrefix %~ (drop bufferSize)
      $ layerCellPrefix   -~ (numSamplesToCells bpm $ fromIntegral bufferSize)
      $ layerCellPrefix   +~ remFrac
      $ layer
    , take bufferSize $ _layerSourcePrefix ++ silence
    )
  | otherwise = ( remLayer, take bufferSize $ samples )
  where
    (remLen, remFrac) = numSamplesForCell bpm _layerCellPrefix
    remSamples = take remLen $ _layerSourcePrefix ++ silence
    (remLayer, newSamples) = getSamples bpm
                                        layer
                                        ( _layerBeat & ix 0 +~ remFrac )
                                        (bufferSize - remLen)
    samples = remSamples ++ newSamples

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
    cellSamps = fromIntegral wholeCellSamps + leftover
    diff = wholeCellSamps - nsamps
    newCellPrefix = c - numSamplesToCells bpm (fromIntegral nsamps) + leftover
    newLayer = layerBeat         .~ cells
             $ layerCellPrefix   .~ newCellPrefix
             $ layerSourcePrefix .~ (drop nsamps source)
             $ layer

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


