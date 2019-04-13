{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Layer
  ( mkLayerWidget
  , renderLayerWidget
  , handleLayerWidgetEvent
  ) where

import Control.Lens
import Control.Monad (guard)

import            Brick ((<+>), (<=>), EventM, Padding(Pad), Widget, clickable, hLimitPercent, handleEventLensed, joinBorders, padAll, padTop, str, withBorderStyle)
import qualified  Brick.Widgets.Edit as E
import qualified  Brick.Focus as Focus
import qualified  Brick.Widgets.Edit as Edit
import qualified  Brick.Widgets.Border as Border
import qualified  Brick.Widgets.Border.Style as Border
import            Graphics.Vty.Input.Events (Event)

import Jambda.Types
import Jambda.Data (linearTaper, modifyBeat, modifyOffset, modifySource, parseBeat, parseCell, parsePitch, resetLayer, silence, sineWave)

mkButton :: String -> Widget Name
mkButton label =
  withBorderStyle Border.unicodeBold . Border.border $ str label

mkLayerWidget :: Int -> Layer -> LayerWidget Name
mkLayerWidget index layer =
  LayerWidget
    { _layerWidgetIndex = index
    , _layerWidgetCodeField = E.editor ( LayerName index BeatCodeName ) ( Just 1 ) ( layer^.layerCode )
    , _layerWidgetOffsetField = E.editor ( LayerName index OffsetName ) ( Just 1 ) ( layer^.layerOffsetCode )
    , _layerWidgetSourceField = E.editor ( LayerName index NoteName ) ( Just 1 ) ( show $ layer^.layerSourceType )
    , _layerWidgetDelete = clickable ( LayerName index DeleteName ) . padAll 1 $ str "[-X-]"
    }

renderLayerWidget :: JamState -> LayerWidget Name -> Widget Name
renderLayerWidget st LayerWidget{..} = joinBorders
                                     . withBorderStyle Border.unicode
                                     . Border.borderWithLabel ( str $ "Layer " <> show ( succ _layerWidgetIndex ) )
                                     $ padAll 1 elements
  where
    beatEditor = drawEditor "Beat" _layerWidgetCodeField
    offsetEditor = hLimitPercent 20 $ drawEditor "Offset" _layerWidgetOffsetField
    sourceEditor = hLimitPercent 15 $ drawEditor "Note" _layerWidgetSourceField
    elements = beatEditor
           <+> offsetEditor
           <+> sourceEditor
           <+> _layerWidgetDelete

    drawEditor label f = withBorderStyle Border.unicodeBold
                       . Border.borderWithLabel ( str label )
                       $ Focus.withFocusRing (st^.jamStFocus) (Edit.renderEditor (str . unlines)) f

handleLayerWidgetEvent :: LayerFieldName -> Event -> Maybe (LayerWidget n) -> EventM n (Maybe (LayerWidget n))
handleLayerWidgetEvent _ _ Nothing = pure Nothing
handleLayerWidgetEvent field ev (Just widget) =
  Just <$> case field of
    BeatCodeName -> handleEventLensed widget layerWidgetCodeField Edit.handleEditorEvent ev
    OffsetName   -> handleEventLensed widget layerWidgetOffsetField Edit.handleEditorEvent ev
    NoteName     -> handleEventLensed widget layerWidgetSourceField Edit.handleEditorEvent ev
    DeleteName   -> pure widget


