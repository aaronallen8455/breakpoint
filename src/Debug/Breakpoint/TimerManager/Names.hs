{-# LANGUAGE CPP #-}
module Debug.Breakpoint.TimerManager.Names
  ( psqModName
  , timerManagerModName
  , pkgName
  ) where

psqModName :: String
psqModName =
#if MIN_VERSION_ghc(9,10,0)
  "GHC.Internal.Event.PSQ"
#else
  "GHC.Event.PSQ"
#endif

timerManagerModName :: String
timerManagerModName =
#if MIN_VERSION_ghc(9,10,0)
  "GHC.Internal.Event.TimerManager"
#else
  "GHC.Event.TimerManager"
#endif

pkgName :: String
pkgName =
#if MIN_VERSION_ghc(9,10,0)
  "ghc-internal"
#else
  "base"
#endif

