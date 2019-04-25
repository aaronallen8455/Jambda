module Jambda.UI.Events.Base
  ( handler
  ) where

import            Control.Lens
import            Control.Applicative (empty)

import qualified  Brick
import qualified  Brick.Focus as Focus
import qualified  Graphics.Vty as Vty

import            Jambda.Types

handler :: JambdaHandler
handler st ( Brick.VtyEvent ev ) = case ev of
  Vty.EvKey (Vty.KChar '`') [] -> halt st
  Vty.EvKey (Vty.KChar '\t') [] -> continue $ st & jamStFocus %~ Focus.focusNext
  Vty.EvKey Vty.KBackTab [] -> continue $ st & jamStFocus %~ Focus.focusPrev
  _ -> empty

handler _ _ = empty

