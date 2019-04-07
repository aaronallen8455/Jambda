{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Layer
  ( modifyBeat
  , modifyOffset
  , modifySource
  , resetLayer
  , mkLayerWidget
  , renderLayerWidget
  , handleLayerWidgetEvent
  , LayerWidget
  ) where

import Control.Lens
import Control.Monad (guard)

import            Brick ((<+>), EventM, Padding(Pad), Widget, clickable, hLimitPercent, handleEventLensed, joinBorders, padAll, padTop, str, withBorderStyle)
import qualified  Brick.Widgets.Edit as E
import qualified  Brick.Focus as Focus
import qualified  Brick.Widgets.Edit as Edit
import qualified  Brick.Widgets.Border as Border
import qualified  Brick.Widgets.Border.Style as Border
import            Graphics.Vty.Input.Events (Event)

import Jambda.Types
import Jambda.Data (linearTaper, parseBeat, parseCell, parsePitch, silence, sineWave)

mkLayerWidget :: Int -> Layer -> LayerWidget Name
mkLayerWidget index layer =
  LayerWidget
    { _layerWidgetIndex = index
    , _layerWidgetCodeField = E.editor ( LayerName index BeatCodeName ) ( Just 1 ) ( layer^.layerCode )
    , _layerWidgetOffsetField = E.editor ( LayerName index OffsetName ) ( Just 1 ) ( layer^.layerOffsetCode )
    , _layerWidgetSourceField = E.editor ( LayerName index NoteName ) ( Just 1 ) ( show $ layer^.layerSourceType )
    , _layerWidgetDelete = clickable ( LayerName index DeleteName ) . padAll 1 $ str "❌"
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

handleLayerWidgetEvent :: LayerFieldName -> Event -> LayerWidget n -> EventM n (LayerWidget n)
handleLayerWidgetEvent field ev widget =
  case field of
    BeatCodeName -> handleEventLensed widget layerWidgetCodeField Edit.handleEditorEvent ev
    OffsetName   -> handleEventLensed widget layerWidgetOffsetField Edit.handleEditorEvent ev
    NoteName     -> handleEventLensed widget layerWidgetSourceField Edit.handleEditorEvent ev
    DeleteName   -> pure widget


-- | Change the beatcode of a Layer
modifyBeat :: Cell -> String -> Layer -> Maybe Layer
modifyBeat elapsedCells beatCode layer = do
  cells <- parseBeat beatCode
  guard $ length cells > 0
  pure . syncLayer elapsedCells $ layer & layerBeat       .~ cycle cells
                                        & layerCode       .~ beatCode
                                        & layerParsedCode .~ cells

-- | Change the offset of the layer
modifyOffset :: Cell -> String -> Layer -> Maybe Layer
modifyOffset elapsedCells offsetCode layer = do
  offset <- parseCell offsetCode
  pure . syncLayer elapsedCells $ layer & layerCellOffset .~ offset
                                        & layerOffsetCode .~ offsetCode

-- | Fast-forward a layer to the current time position
syncLayer :: Cell -> Layer -> Layer
syncLayer elapsedCells layer
  | remainingElapsed <= 0 =
      layer & layerCellPrefix .~ abs remainingElapsed
  | otherwise =
      layer & layerBeat       .~ newCells
            & layerCellPrefix .~ cellPrefix
  where
    remainingElapsed = elapsedCells - layer^.layerCellOffset
    cycleSize = sum $ layer^.layerBeat
    elapsedCycles = remainingElapsed / cycleSize
    wholeCycles = fromIntegral $ truncate elapsedCycles
    cellsToDrop = elapsedCells - wholeCycles * cycleSize
    cellCycle = cycle $ layer^.layerParsedCode
    (cellPrefix, newCells) = dropCells cellsToDrop cellCycle
    dropCells dc (c:cs)
      | c >= dc = (c - dc, cs)
      | otherwise = dropCells (dc - c) cs

-- | Change the sound source (Pitch) of the layer
modifySource :: String -> Layer -> Maybe Layer
modifySource noteStr layer = do
  pitch <- parsePitch noteStr
  let freq = pitchToFreq pitch
      wave = sineWave freq 0
      newSource = linearTaper 0.2 wave

  pure $ layer & layerSource     .~ newSource
               & layerSourceType .~ pitch

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat .~ ( cycle $ layer^.layerParsedCode )
        & layerCellPrefix .~ ( layer^.layerCellOffset )
        & layerSourcePrefix .~ []

