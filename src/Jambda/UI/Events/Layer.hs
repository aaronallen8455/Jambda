{-# LANGUAGE RankNTypes #-}
module Jambda.UI.Events.Layer
  ( handler
  ) where

import            Control.Monad.Trans (MonadTrans, lift)
import            Control.Monad.IO.Class (liftIO)
import            Control.Lens
import            Data.IORef (modifyIORef')

import qualified  Brick
import qualified  Brick.Focus as Focus
import qualified  Data.CircularList as CList
import qualified  Graphics.Vty as Vty

import            Jambda.Types
import            Jambda.Data (applyLayerBeatChange, applyLayerOffsetChange, applyLayerSourceChange)
import            Jambda.UI.Draw (editedAttr, errorAttr)
import            Jambda.UI.Layer (handleLayerWidgetEvent)
import            Jambda.UI.Editor (Editor, setEditorAttr)

handler :: JambdaHandler
handler = mouse
      >|< applyEditorChanges
      >|< editorFieldEdited
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

applyEditorChanges :: JambdaHandler
applyEditorChanges st ( Brick.VtyEvent ( Vty.EvKey Vty.KEnter [] ) )
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

applyEditorChanges _ _ = empty

editorFieldEdited :: JambdaHandler
editorFieldEdited st ( Brick.VtyEvent ev@( Vty.EvKey key _ ) )
  | notKey key = empty
  | Just ( LayerName n BeatCodeName ) <- focus =
    let st' = setEdited layerWidgetCodeField n st
     in handleLensed st' n BeatCodeName ev
  | Just ( LayerName n OffsetName ) <- focus =
    let st' = setEdited layerWidgetOffsetField n st
     in handleLensed st' n OffsetName ev
  | Just ( LayerName n NoteName ) <- focus =
    let st' = setEdited layerWidgetSourceField n st
     in handleLensed st' n NoteName ev
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus
    notKey ( Vty.KChar _ ) = False
    notKey Vty.KBS = False
    notKey Vty.KDel = False
    notKey _ = True

editorFieldEdited _ _ = empty

keystroke :: JambdaHandler
keystroke st ( Brick.VtyEvent ev )
  | Just ( LayerName n field ) <- focus =
      handleLensed st n field ev
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

keystroke _ _ = empty

setEdited :: Lens' (LayerWidget Name) (Editor Name)
          -> Int -> JamState -> JamState
setEdited l n = jamStLayerWidgets.ix n . l
                  %~ setEditorAttr editedAttr

handleLensed :: (MonadTrans m, Monad (m (Brick.EventM Name)))
             => JamState -> Int -> LayerFieldName -> Vty.Event
             -> m (Brick.EventM Name) (Brick.Next JamState)
handleLensed st n field ev =
   ( continue =<< ) . lift $
     Brick.handleEventLensed st
                             ( jamStLayerWidgets . at n )
                             ( handleLayerWidgetEvent field )
                             ev
