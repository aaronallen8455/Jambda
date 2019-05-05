module Jambda.UI.Events.Transport
  ( handler
  ) where

import            Control.Monad.Trans (lift)
import            Control.Monad.IO.Class (liftIO)
import            Control.Lens
import            Data.Functor (void)
import qualified  Data.IntMap as Map
import            Data.IORef (writeIORef, modifyIORef', readIORef)
import qualified  Data.Text.Zipper as TextZipper

import qualified  Data.CircularList as CList
import qualified  Brick
import qualified  Brick.Focus as Focus
import qualified  Graphics.Vty as Vty

import            Jambda.Types
import            Jambda.Data (newLayer, numSamplesToCellValue, parseBpm, resetLayer, syncLayer)
import            Jambda.UI.Layer (mkLayerWidget)
import            Jambda.UI.Editor (applyEdit, getEditorContents, handleEditorEvent, setEditorAttr)
import            Jambda.UI.Draw (errorAttr)

handler :: JambdaHandler
handler = keystroke
      >|< mouse

keystroke :: JambdaHandler
keystroke st ( Brick.VtyEvent ev )
  | Just TempoName <- focus =
      case ev of
        Vty.EvKey Vty.KUp [] -> modifyTempo succ

        Vty.EvKey Vty.KDown [] -> modifyTempo pred

        Vty.EvKey Vty.KEnter [] -> applyTempoChange

        _ ->
          ( continue =<< ) . lift $
            Brick.handleEventLensed st
                                    jamStTempoField
                                    handleEditorEvent
                                    ev
  where
    focus = Focus.focusGetCurrent $ st^.jamStFocus

    modifyTempo f = do
      newTempoStr <- liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
        currentTempo <- readIORef $ st^.jamStTempoRef
        let newTempo = max 1 $ f currentTempo
            ratio = getBPM $ currentTempo / newTempo

        modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
        writeIORef ( st^.jamStTempoRef ) newTempo
        pure $ bpmToString newTempo

      let st' = st & jamStTempoField %~
                     ( applyEdit
                     . const $ TextZipper.stringZipper [ newTempoStr ]
                                                       ( Just 1 )
                     )
      continue st'

    applyTempoChange = do
      let tempoStr = getEditorContents $ st^.jamStTempoField
          mbTempo = parseBpm tempoStr

      case mbTempo of
        Just tempo ->
          fmap ( jamStTempoField %~ setEditorAttr mempty )
            <$> modifyTempo (const tempo)
        Nothing ->
          continue $ st & jamStTempoField %~ setEditorAttr errorAttr

keystroke _ _ = empty

mouse :: JambdaHandler
mouse st ( Brick.MouseDown n _ _ _ ) =
  case n of
    PlayName -> do
      void . liftIO $ st^.jamStStartPlayback
      continue st

    StopName -> do
      void . liftIO $ do
        st^.jamStStopPlayback
        writeIORef ( st^.jamStElapsedSamples ) 0
        modifyIORef' ( st^.jamStLayersRef ) ( fmap resetLayer )
      continue st

    AddLayerName -> do
      st' <- liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
        elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
        tempo <- readIORef ( st^.jamStTempoRef )

        let elapsedCells =
              numSamplesToCellValue tempo
                                    ( fromRational elapsedSamples )
            mbNewIx = succ . fst <$> Map.lookupMax ( st^.jamStLayerWidgets )
            newIx = maybe 0 id mbNewIx
            layer = newLayer $ Pitch ANat 4 :: Layer
            layerWidget = mkLayerWidget newIx layer
            st' = st & jamStLayerWidgets %~ ( at newIx ?~ layerWidget )
                     & jamStFocus
                         %~ Focus.focusRingModify
                              ( CList.insertR ( LayerName newIx BeatCodeName )
                              . CList.insertR ( LayerName newIx OffsetName )
                              . CList.insertR ( LayerName newIx NoteName )
                              )

        void $ modifyIORef' ( st^.jamStLayersRef )
                            ( fmap ( syncLayer elapsedCells )
                            . ( at newIx ?~ layer )
                            )
        pure st'

      continue st'

    TempoName -> do
      continue $ st & jamStFocus %~ Focus.focusSetCurrent ( TempoName )

    _ -> empty

mouse _ _ = empty
