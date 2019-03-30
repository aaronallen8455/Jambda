{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types where

import Control.Lens (makeLenses)
import Data.IORef
import Jambda.Newtypes (Cell, Sample, BPM)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

data Layer =
  Layer
    { _layerSource :: ![Sample]
    , _layerBeat :: ![Cell]
    , _layerDisplay :: !String
    , _layerCellPrefix :: !Cell
    , _layerSourcePrefix :: ![Sample]
    }

makeLenses ''Layer

data Name
  = LayerName Int
  | TempoName
-- | AddLayerName
  deriving (Eq, Ord, Show)

data JambdaState =
  JambdaState
    { _jambdaStateLayersRef :: !(IORef [Layer])
    , _jambdaStateTempoRef :: !(IORef BPM)
    , _jambdaStateVolumeRef :: !(IORef Double)
    , _jambdaStateLayerFields :: ![E.Editor String Name]
    , _jambdaStateTempoField :: !(E.Editor String Name)
    , _jambdaStateFocus :: !(F.FocusRing Name)
    }

makeLenses ''JambdaState
