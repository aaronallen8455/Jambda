{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import Control.Lens (makeLenses)
import Data.IORef

import Jambda.Types.Semaphore (Semaphore)
import Jambda.Types.Layer (Layer)
import Jambda.Types.Newtypes (BPM, Cell, Sample)
import Jambda.Types.LayerWidget (LayerFieldName, LayerWidget)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

data Name
  = LayerName Int LayerFieldName
  | TempoName
  | PlayName
  | StopName
-- | AddLayerName
  deriving (Eq, Ord, Show)

data JamState =
  JamState
    { _jamStLayersRef :: !(IORef [Layer])
    , _jamStTempoRef :: !(IORef BPM)
    , _jamStVolumeRef :: !(IORef Double)
    , _jamStLayerWidgets :: ![LayerWidget Name]
    , _jamStTempoField :: !(E.Editor String Name)
    , _jamStFocus :: !(F.FocusRing Name)
    , _jamStElapsedCells :: !(IORef Cell)
    , _jamStSemaphore :: !Semaphore
    , _jamStStartPlayback :: IO ()
    , _jamStStopPlayback :: IO ()
    }

makeLenses ''JamState
