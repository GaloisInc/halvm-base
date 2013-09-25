{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Trustworthy #-}
module GHC.Event.NoIO(
         ensureIOManagerIsRunning
       , ioManagerCapabilitiesChanged
       , threadDelay
       , registerDelay
       )
 where

import Data.Maybe(Maybe(..))
import Foreign.StablePtr(StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import GHC.Base
import GHC.Conc.Sync(TVar, atomically, newTVar, writeTVar, forkIO)
import GHC.MVar(MVar, newEmptyMVar, takeMVar, putMVar)
import Foreign.C.String
import Foreign.Ptr

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = forkIO runWaiters >> return ()
 where
  runWaiters = do
    next <- waitForWaiter
    action <- deRefStablePtr next
    _ <- forkIO action
    runWaiters

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged  = return ()

threadDelay :: Int -> IO ()
threadDelay usecs = mdo
  wait <- newEmptyMVar
  sp <- newStablePtr $ putMVar wait () >> freeStablePtr sp
  registerWaiter usecs sp
  takeMVar wait

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = mdo
  t <- atomically $ newTVar False
  sp <- newStablePtr $ atomically (writeTVar t True) >> freeStablePtr sp
  return t

foreign import ccall unsafe "registerWaiter"
  registerWaiter :: Int -> StablePtr (IO ()) -> IO ()

foreign import ccall safe "waitForWaiter"
  waitForWaiter :: IO (StablePtr (IO ()))
