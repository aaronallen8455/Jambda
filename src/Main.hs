{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (void)
import qualified  Data.IntMap as Map
import qualified Data.Vector.Storable.Mutable as MV
import Data.IORef
import Control.Lens

import qualified SDL
import qualified Brick as Brick
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Focus as Focus
import qualified Graphics.Vty as Vty

import Jambda.Types
import Jambda.Data (audioCallback, newLayer)
import Jambda.UI (drawUI, eventHandler, mkLayerWidget)

main :: IO ()
main = do
  let layers = Map.singleton 0 ( newLayer $ Pitch ANat 4 )
      tempo = 120
  layerRef          <- newIORef layers
  tempoRef          <- newIORef tempo
  volumeRef         <- newIORef 1
  elapsedSamplesRef <- newIORef 0
  semaphore         <- newSemaphore

  SDL.initialize [SDL.InitAudio]
  (audioDevice, audioSpec)
    <- SDL.openAudioDevice
     $ openDeviceSpec
     $ audioCallback semaphore layerRef tempoRef elapsedSamplesRef

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

      focusRing = Focus.focusRing
                    ( ( LayerName <$> [ 0 .. length layers - 1 ]
                                  <*> [ BeatCodeName, OffsetName, NoteName ]
                      ) ++ [ TempoName ]
                    )

      tempoField = Edit.editor TempoName ( Just 1 )
                                         ( bpmToString tempo )

      initState = JamState { _jamStLayersRef      = layerRef
                           , _jamStTempoRef       = tempoRef
                           , _jamStVolumeRef      = volumeRef
                           , _jamStTempoField     = tempoField
                           , _jamStLayerWidgets   = imap mkLayerWidget layers
                           , _jamStFocus          = focusRing
                           , _jamStElapsedSamples = elapsedSamplesRef
                           , _jamStSemaphore      = semaphore
                           , _jamStStartPlayback  = startPlayback
                           , _jamStStopPlayback   = stopPlayback
                           }

  finalState <- Brick.defaultMain app initState :: IO JamState

  SDL.closeAudioDevice audioDevice
  SDL.quit
  pure ()

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
