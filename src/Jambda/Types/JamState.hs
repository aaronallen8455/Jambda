{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import            Control.Lens (makeLenses)
import            Data.IORef
import qualified  Data.IntMap as M

import            Jambda.Types.Semaphore (Semaphore)
import            Jambda.Types.Layer (Layer)
import            Jambda.Types.Newtypes (BPM)
import            Jambda.Types.LayerWidget (LayerWidget)
import            Jambda.Types.Name (Name)
import            Jambda.UI.Editor (Editor)
import qualified  Brick.Focus as F

data JamState =
  JamState
    { _jamStLayersRef      :: !(IORef (M.IntMap Layer))      -- ^ A reference to a Map of all Layers
    , _jamStTempoRef       :: !(IORef BPM)                   -- ^ Holds reference to the tempo
    , _jamStVolumeRef      :: !(IORef Double)                -- ^ Holds reference to the volume level
    , _jamStLayerWidgets   :: !(M.IntMap (LayerWidget Name)) -- ^ Map of LayerWidgets
    , _jamStTempoField     :: !(Editor Name)        -- ^ The tempo input field
    , _jamStFocus          :: !(F.FocusRing Name)            -- ^ Manages which UI element has focus
    , _jamStElapsedSamples :: !(IORef Rational)              -- ^ Number of samples that have elapsed during playback
    , _jamStSemaphore      :: !Semaphore                     -- ^ Semaphore used to manage concurrency
    , _jamStStartPlayback  :: IO ()                          -- ^ Start playback
    , _jamStStopPlayback   :: IO ()                          -- ^ Stop playback
    }

makeLenses ''JamState
