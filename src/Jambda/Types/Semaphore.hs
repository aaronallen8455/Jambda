module Jambda.Types.Semaphore
  ( Semaphore
  , newSemaphore
  , waitSemaphore
  , signalSemaphore
  ) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, withMVarMasked)

newtype Semaphore = Semaphore (MVar ())

newSemaphore :: IO Semaphore
newSemaphore = Semaphore <$> newMVar ()

waitSemaphore :: Semaphore -> IO ()
waitSemaphore (Semaphore mvar) = readMVar mvar

signalSemaphore :: Semaphore -> IO b -> IO b
signalSemaphore (Semaphore mvar) = withMVarMasked mvar . const
