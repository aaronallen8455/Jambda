{-# LANGUAGE RankNTypes #-}
module Jambda.UI.Events
  ( eventHandler
  ) where

import            Control.Monad (join, void)
import            Control.Monad.IO.Class
import            Data.IORef
import qualified  Data.IntMap as Map

import            Control.Lens
import qualified  Data.CircularList as CList
import qualified  Brick as Brick
import qualified  Brick.Widgets.Edit as Edit
import qualified  Brick.Focus as Focus
import qualified  Graphics.Vty as Vty
import qualified  Data.Text.Zipper as TextZipper

import            Jambda.Types
import            Jambda.Data (modifyBeat, modifyOffset, modifySource, newLayer, numSamplesToCells, resetLayer, syncLayer)
import            Jambda.UI.Layer (handleLayerWidgetEvent, mkLayerWidget)

eventHandler :: JamState -> Brick.BrickEvent Name e -> Brick.EventM Name ( Brick.Next JamState )
eventHandler st (Brick.MouseDown n _ _ _) =
  case n of
    TempoName   -> Brick.continue $ st & jamStFocus %~ Focus.focusSetCurrent TempoName

    LayerName i DeleteName -> do
      liftIO $ modifyIORef ( st^.jamStLayersRef )
                           ( sans i )

      let nameFilter (LayerName li _) | li == i = False
          nameFilter _ = True
          st' = st & jamStFocus %~ ( Focus.focusRingModify ( CList.filterR nameFilter ) )
                   & jamStLayerWidgets %~ sans i

      Brick.continue st'

    LayerName i f -> Brick.continue $ st & jamStFocus %~ Focus.focusSetCurrent ( LayerName i f )

    PlayName -> do
      void . liftIO $ st^.jamStStartPlayback
      Brick.continue st

    StopName -> do
      void . liftIO $ st^.jamStStopPlayback
      void . liftIO $ writeIORef ( st^.jamStElapsedSamples ) 0
      void . liftIO $ modifyIORef' ( st^.jamStLayersRef ) ( fmap resetLayer )
      Brick.continue st

    AddLayerName -> do
      st' <- liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
        elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
        tempo <- readIORef ( st^.jamStTempoRef )

        let elapsedCells = numSamplesToCells tempo ( fromRational elapsedSamples )
            mbNewIx = succ . fst <$> Map.lookupMax ( st^.jamStLayerWidgets )
            newIx = maybe 0 id mbNewIx
            layer = syncLayer elapsedCells . newLayer $ Pitch ANat 4 :: Layer
            layerWidget = mkLayerWidget newIx layer
            st' = st & jamStLayerWidgets %~ ( at newIx ?~ layerWidget )
                     & jamStFocus %~ Focus.focusRingModify
                                       ( CList.insertR ( LayerName newIx BeatCodeName )
                                       . CList.insertR ( LayerName newIx OffsetName )
                                       . CList.insertR ( LayerName newIx NoteName )
                                       )

        void $ modifyIORef' ( st^.jamStLayersRef ) ( at newIx ?~ layer )
        pure st'

      Brick.continue st'

eventHandler st (Brick.VtyEvent ev) =
  case ev of
    Vty.EvKey (Vty.KChar '`') [] -> Brick.halt st
    Vty.EvKey (Vty.KChar '\t') [] -> Brick.continue $ st & jamStFocus %~ Focus.focusNext
    Vty.EvKey Vty.KBackTab [] -> Brick.continue $ st & jamStFocus %~ Focus.focusPrev

    Vty.EvKey Vty.KUp [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just TempoName -> do
          newTempoStr <- liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
            currentTempo <- readIORef $ st^.jamStTempoRef
            let newTempo = succ currentTempo
                ratio = getBPM $ currentTempo / newTempo

            modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
            writeIORef ( st^.jamStTempoRef ) newTempo
            pure $ bpmToString newTempo

          let st' = st & jamStTempoField %~
                         ( Edit.applyEdit
                         . const $ TextZipper.stringZipper [ newTempoStr ]
                                                           ( Just 1 )
                         )

          Brick.continue st'
        _ -> Brick.continue st

    Vty.EvKey Vty.KDown [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just TempoName -> do
          newTempoStr <- liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
            currentTempo <- readIORef $ st^.jamStTempoRef
            let newTempo = pred currentTempo
                ratio = getBPM $ currentTempo / newTempo

            modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
            writeIORef ( st^.jamStTempoRef ) newTempo
            pure $ bpmToString newTempo

          let st' = st & jamStTempoField %~
                         ( Edit.applyEdit
                         . const $ TextZipper.stringZipper [ newTempoStr ]
                                                           ( Just 1 )
                         )

          Brick.continue st'
        _ -> Brick.continue st

    -- TODO factor this out
    Vty.EvKey Vty.KEnter [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just (LayerName i BeatCodeName) -> do
          let mbBeatCode = concat . Edit.getEditContents
                       <$> st ^? jamStLayerWidgets.ix i . layerWidgetCodeField

          liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
            elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
            tempo <- readIORef ( st^.jamStTempoRef )
            let elapsedCells = numSamplesToCells tempo
                             $ fromRational elapsedSamples

            modifyIORef' ( st^.jamStLayersRef )
              $ \layers -> let mbLayer = join $ modifyBeat elapsedCells
                                            <$> mbBeatCode
                                            <*> layers ^? ix i
                            in maybe layers (\x -> layers & ix i .~ x) mbLayer
          Brick.continue st
        Just (LayerName i OffsetName) -> do
          let mbOffsetCode = concat . Edit.getEditContents
                         <$> st ^? jamStLayerWidgets.ix i . layerWidgetOffsetField

          liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
            elapsedSamples <- liftIO $ readIORef ( st^.jamStElapsedSamples )
            tempo <- liftIO $ readIORef ( st^.jamStTempoRef )
            let elapsedCells = numSamplesToCells tempo
                             $ fromRational elapsedSamples

            modifyIORef' ( st^.jamStLayersRef )
              $ \layers -> let mbLayer = join $ modifyOffset elapsedCells
                                            <$> mbOffsetCode
                                            <*> layers ^? ix i
                            in maybe layers (\x -> layers & ix i .~ x) mbLayer
          Brick.continue st

        Just (LayerName i NoteName) -> do
          let mbNoteStr = concat . Edit.getEditContents
                      <$> st ^? jamStLayerWidgets.ix i . layerWidgetSourceField

          liftIO . modifyIORef' ( st^.jamStLayersRef )
            $ \layers -> let mbLayer = join $ modifySource
                                          <$> mbNoteStr
                                          <*> layers ^? ix i
                          in maybe layers (\x -> layers & ix i .~ x) mbLayer

          Brick.continue st

        _ -> Brick.continue st

    _ -> Brick.continue
           =<< case Focus.focusGetCurrent ( st^.jamStFocus ) of
                      Just TempoName ->
                        Brick.handleEventLensed st
                                                jamStTempoField
                                                Edit.handleEditorEvent
                                                ev
                      Just (LayerName n field) ->
                        Brick.handleEventLensed st
                                                (jamStLayerWidgets . at n)
                                                (handleLayerWidgetEvent field)
                                                ev
                      Nothing -> pure st
                      _ -> pure st

eventHandler st _ = Brick.continue st

