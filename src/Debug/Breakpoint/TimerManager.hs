{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Debug.Breakpoint.TimerManager
  ( suspendTimeouts
  ) where

#if defined(mingw32_HOST_OS)
-- Since Windows has its own timeout manager internals, I'm choosing not to support it for now.

suspendTimeouts :: IO a -> IO a
suspendTimeouts = id

#else

import           Control.Concurrent(rtsSupportsBoundThreads)
import           Control.Monad (when)
#if !MIN_VERSION_ghc(9,10,0)
import           Data.Foldable (foldl')
#endif
import           Data.IORef
import           Data.Word (Word64)
import qualified GHC.Clock as Clock
import           GHC.Event
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.IO.Unsafe

import           Debug.Breakpoint.TimerManager.Names

--------------------------------------------------------------------------------
-- Hidden functions imported via TH
--------------------------------------------------------------------------------

psqToList =
  $(pure $ VarE $
      Name (OccName "toList")
           (NameG VarName (PkgName pkgName) (ModName psqModName))
   )

psqAdjust =
  $(pure $ VarE $
      Name (OccName "adjust")
           (NameG VarName (PkgName pkgName) (ModName psqModName))
   )

psqKey =
  $(pure $ VarE $
      Name (OccName "key")
#if MIN_VERSION_ghc(9,8,0)
           (NameG (FldName "E") (PkgName pkgName) (ModName psqModName))
#else
           (NameG VarName (PkgName pkgName) (ModName psqModName))
#endif
   )

-- emTimeouts :: TimerManager -> IORef TimeoutQueue
emTimeouts =
  $(pure $ VarE $
      Name (OccName "emTimeouts")
#if MIN_VERSION_ghc(9,8,0)
           (NameG (FldName "TimerManager") (PkgName pkgName) (ModName timerManagerModName))
#else
           (NameG VarName (PkgName pkgName) (ModName timerManagerModName))
#endif
   )

wakeManager :: TimerManager -> IO ()
wakeManager =
  $(pure $ VarE $
      Name (OccName "wakeManager")
           (NameG VarName (PkgName pkgName) (ModName timerManagerModName))
   )

--------------------------------------------------------------------------------
-- Timeout editing
--------------------------------------------------------------------------------

-- editTimeouts :: TimerManager -> TimeoutEdit -> IO ()
editTimeouts mgr g = do
  atomicModifyIORef' (emTimeouts mgr) f
  wakeManager mgr
  where
    f q = (g q, ())

-- | Modify the times in nanoseconds at which all currently registered timeouts
-- will expire.
modifyTimeouts :: (Word64 -> Word64) -> IO ()
modifyTimeouts f =
  -- This only works for the threaded RTS
  when rtsSupportsBoundThreads $ do
    mgr <- getSystemTimerManager
    editTimeouts mgr $ \pq ->
      let els = psqToList pq
          upd pq' k =
            psqAdjust f k pq'
       in foldl' upd pq (psqKey <$> els)

-- | has the effect of suspending timeouts while an action is occurring. This
-- is only used for GHC >= 9.2 because the semantics are too strange without
-- the ability to freeze the runtime.
suspendTimeouts :: IO a -> IO a
suspendTimeouts action = do
  alreadySuspended <- readIORef timeoutsSuspended
  -- Don't allow nested breakpoints to both modify timeouts
  if alreadySuspended || not rtsSupportsBoundThreads
     then action
     else do
       writeIORef timeoutsSuspended True
       let oneYear = 1000 * 1000000 * 60 * 60 * 24 * 365
       -- Add a large length of time to all timeouts so that they don't immediately
       -- expire when blocking ends
       modifyTimeouts (+ oneYear)
       before <- Clock.getMonotonicTimeNSec
       r <- action
       after <- Clock.getMonotonicTimeNSec
       let elapsed = after - before
       -- Set timeouts back to where they were plus the length of time spent blocking
       modifyTimeouts (subtract $ oneYear - elapsed)
       -- NB: any timeouts registered right before the block or immediately afterwards
       -- would result in strange behavior. Perhaps do an atomic modify of the IORef
       -- holding the timeout queue that covers the whole transaction?
       writeIORef timeoutsSuspended False
       pure r

timeoutsSuspended :: IORef Bool
timeoutsSuspended = unsafePerformIO $ newIORef False
{-# NOINLINE timeoutsSuspended #-}

#endif
