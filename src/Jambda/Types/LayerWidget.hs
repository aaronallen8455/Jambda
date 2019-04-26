{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.LayerWidget where

import Control.Lens

import            Brick (Widget)
import qualified  Brick.Widgets.Edit as E

data LayerWidget n = LayerWidget
  { _layerWidgetId          :: !Int
  , _layerWidgetCodeField   :: !(E.Editor String n)
  , _layerWidgetOffsetField :: !(E.Editor String n)
  , _layerWidgetSourceField :: !(E.Editor String n)
  , _layerWidgetDelete      :: !(Widget n)
  }

makeLenses ''LayerWidget
