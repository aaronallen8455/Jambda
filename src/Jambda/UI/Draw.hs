{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Draw
  ( drawUI
  ) where

import            Control.Lens
import           Brick
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Focus as Focus

import Jambda.Types

drawUI :: JamState -> [Widget Name]
drawUI st = [ui] where
  layerEditors = map drawEditor $ st^.jamStLayerFields
  layerUI      = foldr (<=>) emptyWidget layerEditors
  drawEditor f = Focus.withFocusRing (st^.jamStFocus) (Edit.renderEditor (str . unlines)) f
  tempoField   = drawEditor $ st^.jamStTempoField
  playButton   = clickable PlayName $ mkButton "Play"
  stopButton   = clickable StopName $ mkButton "Stop"
  ui           = layerUI
             <=> tempoField
             <=> ( playButton <+> stopButton )

mkButton :: String -> Widget Name
mkButton label =
  withBorderStyle Border.unicodeBold . Border.border . padLeftRight 1 $ str label
