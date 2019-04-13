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
import Control.Monad (when, void, forever, join)
import Data.Function (fix)
import Data.IORef
import qualified Data.IntMap as Map
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

import Jambda.Types
import Jambda.Data (aggregateChunks, audioCallback, linearTaper, readChunk, sineWave)
import Jambda.UI (drawUI, eventHandler, mkLayerWidget)

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
  let layers = Map.fromList [(0, layer), (1, layer2)]
      tempo = 120
  layerRef       <- newIORef layers
  tempoRef       <- newIORef tempo
  volumeRef      <- newIORef 1
  elapsedCellRef <- newIORef 0
  semaphore      <- newSemaphore

  SDL.initialize [SDL.InitAudio]
  (audioDevice, audioSpec)
    <- SDL.openAudioDevice
     $ openDeviceSpec
     $ audioCallback semaphore layerRef tempoRef elapsedCellRef

  let startPlayback = SDL.setAudioDevicePlaybackState audioDevice SDL.Play
      stopPlayback = SDL.setAudioDevicePlaybackState audioDevice SDL.Pause

  let app :: Brick.App JamState e Name
      app = Brick.App { appDraw = drawUI
                      , appChooseCursor = Focus.focusRingCursor (^.jamStFocus)
                      , appHandleEvent = eventHandler
                      , appStartEvent = \s -> do
                          vty <- Brick.getVtyHandle
                          Vty.setMode (Vty.outputIface vty) Vty.Mouse True
                          pure s
                      , appAttrMap = const $ Brick.attrMap Vty.defAttr []
                      }

      initState = JamState { _jamStLayersRef = layerRef
                           , _jamStTempoRef = tempoRef
                           , _jamStVolumeRef = volumeRef
                           , _jamStTempoField = Edit.editor TempoName ( Just 1 ) ( show $ getBPM tempo )
                           , _jamStLayerWidgets = imap mkLayerWidget layers
                           , _jamStFocus = Focus.focusRing ( ( LayerName <$> [ 0 .. length layers - 1 ]
                                                                         <*> [ BeatCodeName, OffsetName, NoteName ]
                                                             ) ++ [ TempoName ]
                                                           )
                           , _jamStElapsedCells = elapsedCellRef
                           , _jamStSemaphore = semaphore
                           , _jamStStartPlayback = startPlayback
                           , _jamStStopPlayback = stopPlayback
                           }

  finalState <- Brick.defaultMain app initState :: IO JamState

  SDL.closeAudioDevice audioDevice
  SDL.quit
  pure ()



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
    , _layerSourceType = Pitch ANat 4
    , _layerBeat = cycle $ [1, 1/3]
    , _layerCode = "1, 1/3"
    , _layerParsedCode = [1, 1/3]
    , _layerCellPrefix = 0
    , _layerSourcePrefix = []
    , _layerOffsetCode = "0"
    , _layerCellOffset = 0
    }

layer2 :: Layer
layer2 =
  Layer
    { _layerSource = linearTaper 0.2 $ sineWave 550 0
    , _layerSourceType = Pitch DFlat 4
    , _layerBeat = cycle $ [1, 0.5, 0.5]
    , _layerCode = "1, .5, .5"
    , _layerParsedCode = [1, 0.5, 0.5]
    , _layerCellPrefix = 0
    , _layerSourcePrefix = []
    , _layerOffsetCode = "0"
    , _layerCellOffset = 0
    }

