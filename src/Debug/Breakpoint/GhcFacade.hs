{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Debug.Breakpoint.GhcFacade
  ( module Ghc
  , liftedRepName
  , LexicalFastString'
  , mkLexicalFastString
  , fromLexicalFastString
  , collectHsBindBinders'
  , collectPatBinders'
  , noLocA'
  , locA'
  , pattern HsLet'
  , pattern LetStmt'
  , pattern ExplicitList'
  ) where

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Hs.Extension as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Tc.Types as Ghc
import           GHC.Parser.Annotation as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Unit.Module.Name as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (TcPlugin)
import           GHC.Data.FastString as Ghc
import           GHC.Hs.Utils as Ghc
import           GHC.Types.Unique.Set as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Hs.Binds as Ghc
import           GHC.Rename.Bind as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Types.Basic as Ghc
import           GHC.Types.Name.Env as Ghc
import           GHC.Builtin.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Core.Make as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Types.Id as Ghc
import           GHC.Core.InstEnv as Ghc
import           GHC.Core.Class as Ghc hiding (FunDep)
import           GHC.Tc.Utils.TcType as Ghc
import           GHC.Core.Type as Ghc
import           GHC.Core.TyCon as Ghc
import           GHC.Types.TyThing.Ppr as Ghc

#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Driver.Finder as Ghc
import           GHC.Hs.Extension as Ghc
import           GHC.Tc.Types as Ghc
import           GHC.Parser.Annotation as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Module.Name as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (TcPlugin)
import           GHC.Data.FastString as Ghc
import           GHC.Hs.Utils as Ghc
import           GHC.Types.Unique.Set as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Hs.Binds as Ghc
import           GHC.Rename.Bind as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Types.Basic as Ghc
import           GHC.Types.Name.Env as Ghc
import           GHC.Builtin.Types as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Core.Make as Ghc
import           GHC.Tc.Types.Evidence as Ghc
import           GHC.Types.Id as Ghc
import           GHC.Core.InstEnv as Ghc
import           GHC.Core.Class as Ghc hiding (FunDep)
import           GHC.Tc.Utils.TcType as Ghc
import           GHC.Core.Type as Ghc
import           GHC.Core.TyCon as Ghc
import           GHC.Core.Ppr.TyThing as Ghc
import           GHC.Driver.Types as Ghc
import           GHC.Hs.Expr as Ghc
import           GHC.Hs.Pat as Ghc
import           GHC.Hs.Decls as Ghc
#endif

liftedRepName :: Ghc.Name
#if MIN_VERSION_ghc(9,2,0)
liftedRepName = Ghc.getName Ghc.liftedRepTyCon
#elif MIN_VERSION_ghc(9,0,0)
liftedRepName = Ghc.getName Ghc.liftedRepDataCon
#endif

#if MIN_VERSION_ghc(9,2,0)
type LexicalFastString' = Ghc.LexicalFastString
#elif MIN_VERSION_ghc(9,0,0)
type LexicalFastString' = Ghc.FastString
#endif

mkLexicalFastString :: Ghc.FastString -> LexicalFastString'
fromLexicalFastString :: LexicalFastString' -> Ghc.FastString
#if MIN_VERSION_ghc(9,2,0)
mkLexicalFastString = Ghc.LexicalFastString
fromLexicalFastString (Ghc.LexicalFastString fs) = fs
#elif MIN_VERSION_ghc(9,0,0)
mkLexicalFastString = id
fromLexicalFastString = id
#endif

collectHsBindBinders' :: Ghc.HsBindLR Ghc.GhcRn idR -> [Ghc.Name]
collectHsBindBinders' = Ghc.collectHsBindBinders
#if MIN_VERSION_ghc(9,2,0)
                          Ghc.CollNoDictBinders
#endif

collectPatBinders' :: Ghc.LPat Ghc.GhcRn -> [Ghc.Name]
collectPatBinders' = Ghc.collectPatBinders
#if MIN_VERSION_ghc(9,2,0)
                       Ghc.CollNoDictBinders
#endif

noLocA'
#if MIN_VERSION_ghc(9,2,0)
  :: a -> LocatedAn an a
noLocA'
  = noLocA
#else
  :: a -> Located a
noLocA'
  = noLoc
#endif

#if MIN_VERSION_ghc(9,2,0)
locA' :: Ghc.SrcSpanAnn' ann -> Ghc.SrcSpan
locA' = Ghc.locA
#else
locA' :: Ghc.SrcSpan -> Ghc.SrcSpan
locA' = id
#endif

pattern HsLet'
  :: Ghc.XLet Ghc.GhcRn
  -> Ghc.Located (Ghc.HsLocalBinds Ghc.GhcRn)
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.HsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,2,0)
pattern HsLet' x lbinds expr <-
  Ghc.HsLet x (Ghc.L Ghc.noSrcSpan -> lbinds) expr
  where
    HsLet' x (Ghc.L _ binds) expr = Ghc.HsLet x binds expr
#else
pattern HsLet' x lbinds expr = Ghc.HsLet x lbinds expr
#endif

pattern LetStmt'
  :: XLetStmt Ghc.GhcRn Ghc.GhcRn body
  -> Ghc.Located (HsLocalBinds Ghc.GhcRn)
  -> Ghc.StmtLR Ghc.GhcRn Ghc.GhcRn body
#if MIN_VERSION_ghc(9,2,0)
pattern LetStmt' x lbinds <-
  Ghc.LetStmt x (Ghc.L Ghc.noSrcSpan -> lbinds)
  where
    LetStmt' x (Ghc.L _ binds) = Ghc.LetStmt x binds
#else
pattern LetStmt' x lbinds = Ghc.LetStmt x lbinds
#endif

pattern ExplicitList'
  :: Ghc.XExplicitList p
  -> [Ghc.LHsExpr p]
  -> Ghc.HsExpr p
#if MIN_VERSION_ghc(9,2,0)
pattern ExplicitList' x exprs = Ghc.ExplicitList x exprs
#else
pattern ExplicitList' x exprs <- Ghc.ExplicitList x _ exprs
  where
    ExplicitList' x exprs = Ghc.ExplicitList x Nothing exprs
#endif
