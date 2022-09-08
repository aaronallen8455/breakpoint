{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Debug.Breakpoint.TimerManager
  ( modifyTimeouts
  ) where

import           Data.Foldable (foldl')
import           Data.IORef (atomicModifyIORef')
import           Data.Word (Word64)
import           GHC.Event
import           Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------
-- Hidden functions imported via TH
--------------------------------------------------------------------------------

psqToList =
  $(pure $ VarE $
      Name (OccName "toList")
           (NameG VarName (PkgName "base") (ModName "GHC.Event.PSQ"))
   )

psqAdjust =
  $(pure $ VarE $
      Name (OccName "adjust")
           (NameG VarName (PkgName "base") (ModName "GHC.Event.PSQ"))
   )

psqKey =
  $(pure $ VarE $
      Name (OccName "key")
           (NameG VarName (PkgName "base") (ModName "GHC.Event.PSQ"))
   )

-- emTimeouts :: TimerManager -> IORef TimeoutQueue
emTimeouts =
  $(pure $ VarE $
      Name (OccName "emTimeouts")
           (NameG VarName (PkgName "base") (ModName "GHC.Event.TimerManager"))
   )

wakeManager :: TimerManager -> IO ()
wakeManager =
  $(pure $ VarE $
      Name (OccName "wakeManager")
           (NameG VarName (PkgName "base") (ModName "GHC.Event.TimerManager"))
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
modifyTimeouts f = do
  mgr <- getSystemTimerManager
  editTimeouts mgr $ \pq ->
    let els = psqToList pq
        upd pq' k =
          psqAdjust f k pq'
     in foldl' upd pq (psqKey <$> els)
