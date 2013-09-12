{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
module GHC.Event.NoIO(
         ensureIOManagerIsRunning
       , ioManagerCapabilitiesChanged
       , threadDelay
       , registerDelay
       )
 where

import Data.Maybe(Maybe(..))
import Foreign.StablePtr(StablePtr, newStablePtr)
import GHC.Base
import GHC.Conc.Sync(TVar, atomically, newTVar, writeTVar)
import GHC.MVar(MVar, newEmptyMVar, takeMVar, putMVar)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning  = return ()

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged  = return ()

threadDelay :: Int -> IO ()
threadDelay usecs = do
  wait <- newEmptyMVar
  sp <- newStablePtr $ putMVar wait ()
  registerWaiter usecs sp
  takeMVar wait

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
  t <- atomically $ newTVar False
  sp <- newStablePtr $ writeTVar t True
  return t

foreign import ccall unsafe "registerWaiter"
  registerWaiter :: Int -> StablePtr (IO ()) -> IO ()
