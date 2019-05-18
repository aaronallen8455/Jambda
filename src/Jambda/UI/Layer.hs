{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Layer
  ( mkLayerWidget
  , renderLayerWidget
  , handleLayerWidgetEvent
  ) where

import Control.Lens hiding (elements)

import            Brick ((<+>), EventM, Widget, clickable, hLimitPercent, handleEventLensed, joinBorders, padAll, str, withBorderStyle)
import qualified  Brick.Focus as Focus
import qualified  Brick.Widgets.Border as Border
import qualified  Brick.Widgets.Border.Style as Border
import            Graphics.Vty.Input.Events (Event)

import Jambda.Types
import Jambda.UI.Editor (editor, renderEditor, handleEditorEvent)

mkLayerWidget :: Int -> Layer -> LayerWidget Name
mkLayerWidget id' layer =
  LayerWidget
    { _layerWidgetId = id'
    , _layerWidgetCodeField = editor ( LayerName id' BeatCodeName ) ( layer^.layerCode )
    , _layerWidgetOffsetField = editor ( LayerName id' OffsetName ) ( layer^.layerOffsetCode )
    , _layerWidgetSourceField = editor ( LayerName id' NoteName ) ( pitchStr $ layer^.layerSourceType )
    , _layerWidgetDelete = clickable ( LayerName id' DeleteName ) . padAll 1 $ str "[-X-]"
    }

renderLayerWidget :: JamState -> Int -> LayerWidget Name -> Widget Name
renderLayerWidget st ind LayerWidget{..} = joinBorders
                                     . withBorderStyle Border.unicode
                                     . Border.borderWithLabel ( str $ "Layer " <> show ( succ ind ) )
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
                       $ Focus.withFocusRing (st^.jamStFocus) renderEditor f

handleLayerWidgetEvent :: LayerFieldName -> Event -> Maybe (LayerWidget n) -> EventM n (Maybe (LayerWidget n))
handleLayerWidgetEvent _ _ Nothing = pure Nothing
handleLayerWidgetEvent field ev (Just widget) =
  Just <$> case field of
    BeatCodeName -> handleEventLensed widget layerWidgetCodeField handleEditorEvent ev
    OffsetName   -> handleEventLensed widget layerWidgetOffsetField handleEditorEvent ev
    NoteName     -> handleEventLensed widget layerWidgetSourceField handleEditorEvent ev
    DeleteName   -> pure widget


