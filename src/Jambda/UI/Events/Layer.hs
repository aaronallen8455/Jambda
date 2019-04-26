module Jambda.UI.Events.Layer
  ( handler
  ) where

import            Control.Monad.Trans (lift)
import            Control.Monad.IO.Class (liftIO)
import            Control.Monad (join)
import            Control.Lens
import            Data.IORef (modifyIORef', readIORef)

import qualified  Brick
import qualified  Brick.Focus as Focus
import qualified  Brick.Widgets.Edit as Edit
import qualified  Data.CircularList as CList
import qualified  Graphics.Vty as Vty

import            Jambda.Types
import            Jambda.Data (modifyBeat, modifyOffset, modifySource, numSamplesToCellValue, syncLayer)
import            Jambda.UI.Layer (handleLayerWidgetEvent)

handler :: JambdaHandler
handler = mouse
      >|< keystroke

mouse :: JambdaHandler
mouse st ( Brick.MouseDown ( LayerName i DeleteName ) _ _ _ ) = do
  liftIO $ modifyIORef' ( st^.jamStLayersRef )
                        ( sans i )

  let nameFilter (LayerName li _) | li == i = False
      nameFilter _ = True
      st' = st & jamStFocus %~ ( Focus.focusRingModify ( CList.filterR nameFilter ) )
               & jamStLayerWidgets %~ sans i

  continue st'

mouse st ( Brick.MouseDown ( LayerName i f ) _ _ _ ) =
  continue $ st & jamStFocus %~ Focus.focusSetCurrent ( LayerName i f )

mouse _ _ = empty

keystroke :: JambdaHandler
keystroke st ( Brick.VtyEvent ( Vty.EvKey Vty.KEnter [] ) )
  | Just ( LayerName i BeatCodeName ) <- focus = do
      let mbBeatCode = concat . Edit.getEditContents
                   <$> st ^? jamStLayerWidgets.ix i . layerWidgetCodeField

      liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
        elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
        tempo <- readIORef ( st^.jamStTempoRef )
        let elapsedCells = numSamplesToCellValue tempo
                         $ fromRational elapsedSamples

        modifyIORef' ( st^.jamStLayersRef )
          $ \layers -> let mbLayer = join $ modifyBeat
                                        <$> mbBeatCode
                                        <*> layers ^? ix i
                        in maybe layers
                                 ( \x -> syncLayer elapsedCells
                                    <$> ( layers & ix i .~ x )
                                 )
                                 mbLayer
      continue st

  | Just ( LayerName i OffsetName ) <- focus = do
      let mbOffsetCode = concat . Edit.getEditContents
                     <$> st ^? jamStLayerWidgets.ix i . layerWidgetOffsetField

      liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
        elapsedSamples <- liftIO $ readIORef ( st^.jamStElapsedSamples )
        tempo <- liftIO $ readIORef ( st^.jamStTempoRef )
        let elapsedCells = numSamplesToCellValue tempo
                         $ fromRational elapsedSamples

        modifyIORef' ( st^.jamStLayersRef )
          $ \layers -> let mbLayer = join $ modifyOffset
                                        <$> mbOffsetCode
                                        <*> layers ^? ix i
                        in maybe layers
                                 ( \x -> syncLayer elapsedCells
                                    <$> ( layers & ix i .~ x )
                                 )
                                 mbLayer
      continue st

  | Just ( LayerName i NoteName ) <- focus = do
      let mbNoteStr = concat . Edit.getEditContents
                  <$> st ^? jamStLayerWidgets.ix i . layerWidgetSourceField

      liftIO . modifyIORef' ( st^.jamStLayersRef )
        $ \layers -> let mbLayer = join $ modifySource
                                      <$> mbNoteStr
                                      <*> layers ^? ix i
                      in maybe layers ( \x -> layers & ix i .~ x ) mbLayer

      continue st
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

keystroke st ( Brick.VtyEvent ev )
  | Just ( LayerName n field ) <- focus =
      ( continue =<< ) . lift $
        Brick.handleEventLensed st
                                (jamStLayerWidgets . at n)
                                (handleLayerWidgetEvent field)
                                ev
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

keystroke _ _ = empty

