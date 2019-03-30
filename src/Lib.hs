{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Lib
  ( sineWave
  , linearTaper
  , test
  , testSDL
  , brickTest
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad (when, void, forever)
import Data.Function (fix)
import Data.IORef
import System.IO (hReady, hSetEcho, stdin)
import GHC.Float (double2Float, float2Double)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL
import qualified Data.Vector.Storable.Mutable as MV
import qualified Brick as Brick
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Focus as Focus
import qualified Graphics.Vty as Vty

import Data.List

import Jambda.Newtypes (BPM(..), Cell(..), Freq(..), Sample(..), Sec(..))
import Jambda.Types

import Debug.Trace

import Data.IORef

testSDL :: BPM -> [Layer] -> IO ()
testSDL bpm layers = do
  layerRef <- newIORef layers
  tempoRef <- newIORef bpm
  SDL.initialize [SDL.InitAudio]
  (audioDevice, audioSpec) <- SDL.openAudioDevice ( openDeviceSpec $ testCB layerRef tempoRef )
  SDL.setAudioDevicePlaybackState audioDevice SDL.Play

  hSetEcho stdin False
  fix $ \rec -> do
    key <- getKey
    case key of
      "\ESC[A" -> modifyIORef tempoRef succ >> rec
      "\ESC[B" -> modifyIORef tempoRef pred >> rec
      "x" -> pure ()
      _ -> rec

  SDL.closeAudioDevice audioDevice
  SDL.quit

openDeviceSpec :: (forall s. SDL.AudioFormat s -> MV.IOVector s -> IO ()) -> SDL.OpenDeviceSpec
openDeviceSpec callback = SDL.OpenDeviceSpec
  { openDeviceFreq = SDL.Mandate 44100
    -- ^ The output audio frequency in herts.
  , openDeviceFormat = SDL.Mandate SDL.FloatingNativeAudio
    -- ^ The format of audio that will be sampled from the output buffer.
  , openDeviceChannels = SDL.Mandate SDL.Stereo
    -- ^ The amount of audio channels.
  , openDeviceSamples = 1024
    -- ^ Output audio buffer size in samples. This should be a power of 2.
  , openDeviceCallback = callback
    -- ^ A callback to invoke whenever new sample data is required. The callback
    -- will be passed a single 'MV.MVector' that must be filled with audio data.
  , openDeviceUsage = SDL.ForPlayback
    -- ^ How you intend to use the opened 'AudioDevice' - either for outputting
    -- or capturing audio.
  , openDeviceName = Nothing
    -- ^ The name of the 'AudioDevice' that should be opened. If 'Nothing',
    -- any suitable 'AudioDevice' will be used.
  }

testCB :: IORef [Layer] -> IORef BPM -> SDL.AudioFormat actualSampleType -> MV.IOVector actualSampleType -> IO ()
testCB layersRef bpmRef SDL.FloatingLEAudio vec = do
  layers <- readIORef layersRef
  bpm <- readIORef bpmRef

  let numSamples = MV.length vec `div` 2
      (newLayers, samples) = unzip $ map (readChunk numSamples bpm) layers
      combined = map getSample $ aggregateChunks samples
  iforM_ combined $ \i s -> do
    MV.write vec (i * 2) s -- Left channel
    MV.write vec (i * 2 + 1) s -- Right channel

  writeIORef layersRef newLayers

testCB _ _ f vec = do
  traceShow f pure ()

test :: IO ()
test = testSDL (BPM 120) [layer, layer2]

brickTest :: IO ()
brickTest = do
  let layers = [layer, layer2]
      tempo = 120
  layerRef <- newIORef layers
  tempoRef <- newIORef tempo
  volumeRef <- newIORef 1

  let app :: Brick.App JambdaState e Name
      app = Brick.App { appDraw = drawUI
                      , appChooseCursor = Focus.focusRingCursor (^.jambdaStateFocus)
                      , appHandleEvent = eventHandler
                      , appStartEvent = pure
                      , appAttrMap = const $ Brick.attrMap Vty.defAttr []
                      }

      mkLayerField i layer = Edit.editor ( LayerName i ) (Just 1 ) (layer^.layerDisplay)

      initState = JambdaState { _jambdaStateLayersRef = layerRef
                              , _jambdaStateTempoRef = tempoRef
                              , _jambdaStateVolumeRef = volumeRef
                              , _jambdaStateTempoField = Edit.editor TempoName ( Just 1 ) ( show $ getBPM tempo )
                              , _jambdaStateLayerFields = imap mkLayerField layers
                              , _jambdaStateFocus = Focus.focusRing (map LayerName [0 .. length layers - 1] ++ [TempoName])
                              }

  finalState <- Brick.defaultMain app initState :: IO JambdaState
  pure ()

drawUI :: JambdaState -> [Brick.Widget Name]
drawUI st = [ui] where
  layerEditors = map drawEditor $ st^.jambdaStateLayerFields
  drawEditor f = Focus.withFocusRing (st^.jambdaStateFocus) (Edit.renderEditor (Brick.str . unlines)) f
  tempoField = drawEditor $ st^.jambdaStateTempoField
  ui = foldr (Brick.<=>) tempoField layerEditors

eventHandler :: JambdaState -> Brick.BrickEvent Name e -> Brick.EventM Name ( Brick.Next JambdaState )
eventHandler st (Brick.VtyEvent ev) =
  case ev of
    Vty.EvKey (Vty.KChar '`') [] -> Brick.halt st
    Vty.EvKey (Vty.KChar '\t') [] -> Brick.continue $ st & jambdaStateFocus %~ Focus.focusNext
    Vty.EvKey Vty.KBackTab [] -> Brick.continue $ st & jambdaStateFocus %~ Focus.focusPrev

    _ -> Brick.continue =<< case Focus.focusGetCurrent ( st^.jambdaStateFocus ) of
                              Just TempoName -> Brick.handleEventLensed st jambdaStateTempoField Edit.handleEditorEvent ev
                              Just (LayerName n) -> Brick.handleEventLensed st (jambdaStateLayerFields . unsafeIx n) Edit.handleEditorEvent ev
                              Nothing -> pure st

unsafeIx :: Int -> Lens' [a] a
unsafeIx n afb s = fmap setAt . afb $ s !! n where
  setAt a = b ++ a : rest
  (b, _: rest) = splitAt n s

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
    , _layerDisplay = "1, 1/3"
    , _layerCellPrefix = 0
    , _layerSourcePrefix = []
    }

layer2 :: Layer
layer2 =
  Layer
    { _layerSource = linearTaper 0.2 $ sineWave 550 0
    , _layerBeat = cycle $ [1, 0.5, 0.5]
    , _layerDisplay = "1, .5, .5"
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

readChunk :: Int -> BPM -> Layer -> (Layer, [Sample])
readChunk bufferSize bpm layer@Layer{..}
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


