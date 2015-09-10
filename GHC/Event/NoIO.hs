{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Trustworthy #-}
module GHC.Event.NoIO(
         ensureIOManagerIsRunning
       , ioManagerCapabilitiesChanged
       , threadDelay
       , registerDelay
       , threadWaitRead
       , threadWaitReadSTM
       , threadWaitWrite
       , threadWaitWriteSTM
       , closeFdWith
       )
 where

import Data.Maybe(Maybe(..))
import Foreign.Marshal.Alloc
import Foreign.Storable(peek)
import Foreign.StablePtr(StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import GHC.Base
import GHC.Conc.Sync(TVar, atomically, newTVar, writeTVar, forkIO, STM, yield)
import GHC.MVar(MVar, newEmptyMVar, takeMVar, putMVar)
import Foreign.C.String
import Foreign.Ptr
import System.Posix.Types(Fd)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning =
  do ptr <- malloc
     _   <- forkIO (ioManager ptr)
     return ()

ioManager :: Ptr (StablePtr (IO ())) -> IO ()
ioManager ptr =
  forever $ do waitTime <- waitForWaiter ptr
               if waitTime == 0
                  then runWaiter
                  else do yield
                          waitTime <- waitForWaiter ptr
                          if waitTime == 0
                             then runWaiter
                             else sleepUntilWaiter waitTime
 where
  runWaiter =
    do sp     <- peek ptr
       action <- deRefStablePtr sp
       _      <- forkIO action
       return ()

forever     :: (Monad m) => m a -> m b
{-# INLINE forever #-}
forever a   = let a' = a >> a' in a'

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged  = return ()

-- The following two functions are obvious candidates for mdo/fixIO,
-- but importing either causes circular dependency problems
threadDelay :: Int -> IO ()
threadDelay usecs =
  do wait <- newEmptyMVar
     spMV <- newEmptyMVar
     sp   <- newStablePtr (do putMVar wait ()
                              sp' <- takeMVar spMV
                              freeStablePtr sp')
     putMVar spMV sp
     registerWaiter usecs sp
     takeMVar wait

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = 
  do t    <- atomically $ newTVar False
     spMV <- newEmptyMVar
     sp   <- newStablePtr (do atomically (writeTVar t True) 
                              sp' <- takeMVar spMV
                              freeStablePtr sp')
     putMVar spMV sp
     registerWaiter usecs sp
     return t

threadWaitRead :: Fd -> IO ()
threadWaitRead _ = return ()

threadWaitWrite :: Fd -> IO ()
threadWaitWrite _ = return ()

threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM _ = return (return (), return ())

threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM _ = return (return (), return ())

closeFdWith :: (Fd -> IO ()) -> Fd -> IO ()
closeFdWith close fd = close fd

foreign import ccall unsafe "registerWaiter"
  registerWaiter :: Int -> StablePtr (IO ()) -> IO ()

foreign import ccall unsafe "waitForWaiter"
  waitForWaiter :: Ptr (StablePtr (IO ())) -> IO Word

foreign import ccall safe "sleepUntilWaiter"
  sleepUntilWaiter :: Word -> IO ()
