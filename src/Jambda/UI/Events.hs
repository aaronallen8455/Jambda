{-# LANGUAGE RankNTypes #-}
module Jambda.UI.Events
  ( eventHandler
  ) where

import            Control.Monad (join, void)
import            Control.Monad.IO.Class
import            Data.IORef

import            Control.Lens
import qualified  Brick as Brick
import qualified  Brick.Widgets.Edit as Edit
import qualified  Brick.Focus as Focus
import qualified  Graphics.Vty as Vty
import qualified  Data.Text.Zipper as TextZipper

import            Jambda.Types
import            Jambda.UI.Layer (modifyBeat, resetLayer)

import Debug.Trace

eventHandler :: JamState -> Brick.BrickEvent Name e -> Brick.EventM Name ( Brick.Next JamState )
eventHandler st (Brick.MouseDown n _ _ _) =
  case n of
    TempoName   -> Brick.continue $ st & jamStFocus %~ Focus.focusSetCurrent TempoName
    LayerName i -> Brick.continue $ st & jamStFocus %~ Focus.focusSetCurrent ( LayerName i )
    PlayName -> do
      void . liftIO $ st^.jamStStartPlayback
      Brick.continue st
    StopName -> do
      void . liftIO $ st^.jamStStopPlayback
      void . liftIO $ writeIORef ( st^.jamStElapsedCells ) 0
      void . liftIO $ modifyIORef' ( st^.jamStLayersRef ) ( map resetLayer )
      Brick.continue st
    -- x -> traceShow x Brick.halt st
eventHandler st (Brick.VtyEvent ev) =
  case ev of
    Vty.EvKey (Vty.KChar '`') [] -> Brick.halt st
    Vty.EvKey (Vty.KChar '\t') [] -> Brick.continue $ st & jamStFocus %~ Focus.focusNext
    Vty.EvKey Vty.KBackTab [] -> Brick.continue $ st & jamStFocus %~ Focus.focusPrev

    Vty.EvKey Vty.KUp [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just TempoName -> do
          liftIO $ modifyIORef' ( st^.jamStTempoRef ) succ
          tempo <- liftIO $ bpmToString <$> readIORef ( st^.jamStTempoRef )
          let st' = st & jamStTempoField %~ Edit.applyEdit ( const $ TextZipper.stringZipper [ tempo ] ( Just 1 ) )
          Brick.continue st'
        _ -> Brick.continue st

    Vty.EvKey Vty.KDown [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just TempoName -> do
          liftIO $ modifyIORef ( st^.jamStTempoRef ) pred
          tempo <- liftIO $ bpmToString <$> readIORef ( st^.jamStTempoRef )
          let st' = st & jamStTempoField %~ Edit.applyEdit ( const $ TextZipper.stringZipper [ tempo ] ( Just 1 ) )
          Brick.continue st'
        _ -> Brick.continue st

    Vty.EvKey Vty.KEnter [] ->
      case Focus.focusGetCurrent ( st^.jamStFocus ) of
        Just (LayerName i) -> do
          let mbBeatCode = concat . Edit.getEditContents <$> st ^? jamStLayerFields.ix i

          liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
            elapsedCells <- liftIO $ readIORef ( st^.jamStElapsedCells )

            modifyIORef' ( st^.jamStLayersRef )
              $ \layers -> let mbLayer = join $ modifyBeat elapsedCells
                                            <$> mbBeatCode
                                            <*> layers ^? ix i
                            in maybe layers (\x -> layers & ix i .~ x) mbLayer
          Brick.continue st
        _ -> Brick.continue st

    _ -> Brick.continue =<< case Focus.focusGetCurrent ( st^.jamStFocus ) of
                              Just TempoName -> Brick.handleEventLensed st jamStTempoField Edit.handleEditorEvent ev
                              Just (LayerName n) -> Brick.handleEventLensed st (jamStLayerFields . unsafeIx n) Edit.handleEditorEvent ev
                              Nothing -> pure st
eventHandler st _ = Brick.continue st

unsafeIx :: Int -> Lens' [a] a
unsafeIx n afb s = fmap setAt . afb $ s !! n where
  setAt a = b ++ a : rest
  (b, _: rest) = splitAt n s
