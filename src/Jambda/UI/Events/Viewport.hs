module Jambda.UI.Events.Viewport
  ( handler
  ) where

import            Control.Monad.Trans (lift)

import            Brick hiding (continue)
import qualified  Graphics.Vty as Vty

import            Jambda.Types

handler :: JambdaHandler
handler = keyboardHandler
      >|< mouseHandler

keyboardHandler :: JambdaHandler
keyboardHandler st ( Brick.VtyEvent ( Vty.EvKey Vty.KUp [] ) ) = do
  let vp = viewportScroll Viewport
  lift ( vScrollBy vp ( -1 ) )
  continue st

keyboardHandler st ( Brick.VtyEvent ( Vty.EvKey Vty.KDown [] ) ) = do
  let vp = viewportScroll Viewport
  lift ( vScrollBy vp 1 )
  continue st

keyboardHandler _ _ = empty

mouseHandler :: JambdaHandler
mouseHandler st ( Brick.VtyEvent ( Vty.EvMouseDown _ _ Vty.BScrollUp [] ) ) = do
  let vp = viewportScroll Viewport
  lift $ vScrollBy vp ( -1 )
  continue st

mouseHandler st ( Brick.VtyEvent ( Vty.EvMouseDown _ _ Vty.BScrollDown [] ) ) = do
  let vp = viewportScroll Viewport
  lift $ vScrollBy vp 1
  continue st

mouseHandler _ _ = empty
