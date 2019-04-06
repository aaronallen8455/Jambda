{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import Control.Lens (makeLenses)
import Data.IORef

import Jambda.Types.Semaphore (Semaphore)
import Jambda.Types.Layer (Layer)
import Jambda.Types.Newtypes (BPM, Cell, Sample)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

data Name
  = LayerName Int
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
    , _jamStLayerFields :: ![E.Editor String Name]
    , _jamStTempoField :: !(E.Editor String Name)
    , _jamStFocus :: !(F.FocusRing Name)
    , _jamStElapsedCells :: !(IORef Cell)
    , _jamStSemaphore :: !Semaphore
    , _jamStStartPlayback :: IO ()
    , _jamStStopPlayback :: IO ()
    }

makeLenses ''JamState
