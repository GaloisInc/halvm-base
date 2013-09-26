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

-- The following two functions are obvious candidates for mdo/fixIO,
-- but importing either causes circular dependency problems
threadDelay :: Int -> IO ()
threadDelay usecs = do
  wait <- newEmptyMVar
  spMV <- newEmptyMVar
  sp <- newStablePtr $ do
          putMVar wait ()
          takeMVar spMV >>= freeStablePtr
  putMVar spMV sp
  registerWaiter usecs sp
  takeMVar wait

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
  t <- atomically $ newTVar False
  spMV <- newEmptyMVar
  sp <- newStablePtr $ do
          atomically $ writeTVar t True
          takeMVar spMV >>= freeStablePtr
  putMVar spMV sp
  registerWaiter usecs sp
  return t

foreign import ccall unsafe "registerWaiter"
  registerWaiter :: Int -> StablePtr (IO ()) -> IO ()

foreign import ccall safe "waitForWaiter"
  waitForWaiter :: IO (StablePtr (IO ()))
