{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Jambda.Types.EventHandler
  ( JambdaHandler
  , runEventHandler
  , continue
  , halt
  , empty
  , (>|<)
  ) where

import            Control.Applicative ((<|>), empty)
import            Control.Monad.Trans.Maybe
import            Control.Monad.Trans (MonadTrans, lift)

import qualified  Brick

import            Jambda.Types.JamState (JamState)
import            Jambda.Types.Name (Name)

type Handler st n = MaybeT ( Brick.EventM n ) ( Brick.Next st )

type EventHandler st n = forall e. st -> Brick.BrickEvent n e -> Handler st n

type JambdaHandler = EventHandler JamState Name

runEventHandler :: EventHandler st n -> st -> Brick.BrickEvent n e -> Brick.EventM n ( Brick.Next st )
runEventHandler h st ev =
  maybe ( Brick.continue st ) pure =<< ( runMaybeT $ h st ev )

continue :: MonadTrans m => st -> m ( Brick.EventM n ) ( Brick.Next st )
continue = lift . Brick.continue

halt :: MonadTrans m => st -> m ( Brick.EventM n ) ( Brick.Next st )
halt = lift . Brick.halt

{-# INLINE (>|<) #-}
(>|<) :: EventHandler st n -> EventHandler st n -> EventHandler st n
a >|< b = \st ev -> a st ev <|> b st ev
