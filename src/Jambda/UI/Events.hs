module Jambda.UI.Events
  ( eventHandler
  ) where

import qualified  Brick

import            Jambda.Types
import qualified  Jambda.UI.Events.Base as Base
import qualified  Jambda.UI.Events.Layer as Layer
import qualified  Jambda.UI.Events.Transport as Transport
import qualified  Jambda.UI.Events.Viewport as Viewport

eventHandler :: JamState
             -> Brick.BrickEvent Name e
             -> Brick.EventM Name ( Brick.Next JamState )
eventHandler = runEventHandler handler

handler :: JambdaHandler
handler = Base.handler
      >|< Transport.handler
      >|< Viewport.handler
      >|< Layer.handler
