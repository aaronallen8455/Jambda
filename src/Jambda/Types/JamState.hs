{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import            Control.Lens (makeLenses)
import            Data.IORef
import qualified  Data.IntMap as M

import            Jambda.Types.Semaphore (Semaphore)
import            Jambda.Types.Layer (Layer)
import            Jambda.Types.Newtypes (BPM)
import            Jambda.Types.LayerWidget (LayerFieldName, LayerWidget)
import qualified  Brick.Widgets.Edit as E
import qualified  Brick.Focus as F

data Name
  = LayerName !Int !LayerFieldName
  | TempoName
  | PlayName
  | StopName
  | AddLayerName
  deriving (Eq, Ord, Show)

data JamState =
  JamState
    { _jamStLayersRef      :: !(IORef (M.IntMap Layer))
    , _jamStTempoRef       :: !(IORef BPM)
    , _jamStVolumeRef      :: !(IORef Double)
    , _jamStLayerWidgets   :: !(M.IntMap (LayerWidget Name))
    , _jamStTempoField     :: !(E.Editor String Name)
    , _jamStFocus          :: !(F.FocusRing Name)
    , _jamStElapsedSamples :: !(IORef Rational)
    , _jamStSemaphore      :: !Semaphore
    , _jamStStartPlayback  :: IO ()
    , _jamStStopPlayback   :: IO ()
    }

makeLenses ''JamState
