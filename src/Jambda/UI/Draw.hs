{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Draw
  ( drawUI
  ) where

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
  layerEditors = renderLayerWidget st <$> st^.jamStLayerWidgets
  layerUI      = foldr (<=>) emptyWidget layerEditors
  drawEditor label f = withBorderStyle Border.unicode
                     . Border.borderWithLabel ( str label )
                     $ Focus.withFocusRing (st^.jamStFocus)
                                           (Edit.renderEditor (str . unlines))
                                           f
  tempoField     = hLimit 12 $ drawEditor "Tempo" $ st^.jamStTempoField
  playButton     = clickable PlayName $ mkButton "Play"
  stopButton     = clickable StopName $ mkButton "Stop"
  addLayerButton = clickable AddLayerName $ mkButton "Add Layer"
  ui           = layerUI
             <=> tempoField
             <=> ( playButton <+> stopButton )
             <=> addLayerButton

mkButton :: String -> Widget Name
mkButton label =
  withBorderStyle Border.unicodeBold . Border.border . padLeftRight 1 $ str label


