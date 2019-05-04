{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.LayerWidget where

import Control.Lens

import            Brick (Widget)

import            Jambda.UI.Editor

data LayerWidget n = LayerWidget
  { _layerWidgetId          :: !Int
  , _layerWidgetCodeField   :: !(Editor n)
  , _layerWidgetOffsetField :: !(Editor n)
  , _layerWidgetSourceField :: !(Editor n)
  , _layerWidgetDelete      :: !(Widget n)
  }

makeLenses ''LayerWidget
