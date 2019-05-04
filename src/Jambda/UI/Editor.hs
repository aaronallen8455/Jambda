{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Jambda.UI.Editor
  ( Editor
  , editorAttr
  , editorWidget
  , handleEditorEvent
  , setEditorAttr
  , editor
  , renderEditor
  , getEditorContents
  , applyEdit
  ) where

import            Control.Lens (makeLenses, (%~))
import            Data.Text.Zipper (TextZipper)

import            Brick
import qualified  Brick.Widgets.Edit as E
import            Graphics.Vty (Event)

data Editor n = Editor
  { _editorAttr :: AttrName
  , _editorWidget :: E.Editor String n
  }

makeLenses ''Editor

handleEditorEvent :: Event -> Editor n -> EventM n (Editor n)
handleEditorEvent event ed@Editor{..} =
    fmap wrap $ E.handleEditorEvent event _editorWidget
  where
    wrap e = ed { _editorWidget = e }

setEditorAttr :: AttrName -> Editor n -> Editor n
setEditorAttr attr e = e { _editorAttr = attr }

editor :: n -> String -> Editor n
editor name content =
  Editor
    { _editorAttr = mempty
    , _editorWidget = E.editor name ( Just 1 ) content
    }

renderEditor :: (Show n, Ord n) => Bool -> Editor n -> Widget n
renderEditor hasFocus Editor{..}
  = withDefAttr _editorAttr
  $ E.renderEditor ( str . unlines ) hasFocus _editorWidget

getEditorContents :: Editor n -> String
getEditorContents = concat . E.getEditContents . _editorWidget

applyEdit :: (TextZipper String -> TextZipper String) -> Editor n -> Editor n
applyEdit modifier = editorWidget %~ E.applyEdit modifier

instance Named (Editor n) n where
  getName Editor{..} = getName _editorWidget
