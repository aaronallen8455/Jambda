module Jambda.UI.Events.Layer
  ( handler
  ) where

import            Control.Monad.Trans (lift)
import            Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import            Control.Monad.IO.Class (liftIO)
import            Control.Monad (join)
import            Control.Lens
import            Data.Maybe (isJust)
import            Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified  Data.Stream.Infinite as Stream

import qualified  Brick
import qualified  Brick.Focus as Focus
import qualified  Data.CircularList as CList
import qualified  Graphics.Vty as Vty

import            Jambda.Types
import            Jambda.Data (applyLayerBeatChange, applyLayerOffsetChange, applyLayerSourceChange)
import            Jambda.UI.Draw (errorAttr)
import            Jambda.UI.Layer (handleLayerWidgetEvent)
import            Jambda.UI.Editor (getEditorContents, setEditorAttr)

handler :: JambdaHandler
handler = mouse
      >|< keystroke

mouse :: JambdaHandler
mouse st ( Brick.MouseDown ( LayerName i DeleteName ) _ _ _ ) = do
  liftIO $ modifyIORef' ( st^.jamStLayersRef )
                        ( sans i )

  let nameFilter (LayerName li _) | li == i = False
      nameFilter _ = True
      st' = st & jamStFocus %~ ( Focus.focusRingModify
                                   ( CList.filterR nameFilter )
                               )
               & jamStLayerWidgets %~ sans i

  continue st'

mouse st ( Brick.MouseDown ( LayerName i f ) _ _ _ ) =
  continue $ st & jamStFocus %~ Focus.focusSetCurrent ( LayerName i f )

mouse _ _ = empty

keystroke :: JambdaHandler
keystroke st ( Brick.VtyEvent ( Vty.EvKey Vty.KEnter [] ) )
  | Just ( LayerName i BeatCodeName ) <- focus = do
      isValid <- liftIO $ applyLayerBeatChange st i

      continue $
        st & jamStLayerWidgets.ix i . layerWidgetCodeField
               %~ setEditorAttr ( if isValid then mempty else errorAttr )

  | Just ( LayerName i OffsetName ) <- focus = do
      isValid <- liftIO $ applyLayerOffsetChange st i

      continue $
        st & jamStLayerWidgets.ix i . layerWidgetOffsetField
               %~ setEditorAttr ( if isValid then mempty else errorAttr )

  | Just ( LayerName i NoteName ) <- focus = do
      isValid <- liftIO $ applyLayerSourceChange st i

      continue $
        st & jamStLayerWidgets.ix i . layerWidgetSourceField
               %~ setEditorAttr ( if isValid then mempty else errorAttr )
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

keystroke st ( Brick.VtyEvent ev )
  | Just ( LayerName n field ) <- focus =
      ( continue =<< ) . lift $
        Brick.handleEventLensed st
                                ( jamStLayerWidgets . at n )
                                ( handleLayerWidgetEvent field )
                                ev
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

keystroke _ _ = empty

