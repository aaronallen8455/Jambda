{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Draw
  ( drawUI
  ) where

import qualified  Data.IntMap as Map
import            Control.Lens

import            Brick
import qualified  Brick.Widgets.Edit as Edit
import qualified  Brick.Widgets.Border as Border
import qualified  Brick.Widgets.Border.Style as Border
import qualified  Brick.Focus as Focus

import Jambda.Types
import Jambda.UI.Layer (renderLayerWidget)

drawUI :: JamState -> [Widget Name]
drawUI st = [ui] where
  layerEditors       = map ( uncurry $ renderLayerWidget st ) . zip [0..] $ Map.elems ( st^.jamStLayerWidgets )
  layerUI            = foldr (<=>) emptyWidget layerEditors
  drawEditor label f = withBorderStyle Border.unicode
                     . Border.borderWithLabel ( str label )
                     $ Focus.withFocusRing (st^.jamStFocus)
                                           (Edit.renderEditor (str . unlines))
                                           f
  tempoField     = hLimit 12 $ drawEditor "Tempo" $ st^.jamStTempoField
  playButton     = mkButton "Play" PlayName
  stopButton     = mkButton "Stop" StopName
  addLayerButton = mkButton "Add Layer" AddLayerName
  ui           = layerUI
             <=> ( tempoField <+> playButton <+> stopButton <+> addLayerButton )

mkButton :: String -> Name -> Widget Name
mkButton label name
  = withBorderStyle Border.unicodeBold
  . clickable name
  . Border.border
  . padLeftRight 1
  $ str label

