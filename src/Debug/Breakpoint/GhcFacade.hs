{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Debug.Breakpoint.GhcFacade
  ( module Ghc
  , liftedRepName
  , mkLexicalFastString
  , fromLexicalFastString
  , collectHsBindBinders'
  , collectPatBinders'
  , mkWildValBinder'
  , pprTypeForUser'
  , showSDocOneLine'
  , findImportedModule'
  , findPluginModule'
  , pattern HsLet'
  , pattern LetStmt'
  , pattern ExplicitList'
  , pattern OverLit'
  , pattern CDictCan'
  ) where

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Hs.Extension as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Tc.Types as Ghc hiding (DefaultingPlugin)
import qualified GHC.Tc.Plugin as Plugin
import           GHC.Parser.Annotation as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Data.FastString as Ghc
import           GHC.Hs.Utils as Ghc
import           GHC.Types.Unique.Set as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Hs.Binds as Ghc
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
import           GHC.Hs.Expr as Ghc
import           GHC.Types.PkgQual as Ghc
import           GHC.Tc.Types.Origin as Ghc

#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Hs.Extension as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Tc.Types as Ghc hiding (DefaultingPlugin)
import qualified GHC.Tc.Plugin as Plugin
import           GHC.Parser.Annotation as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Iface.Env as Ghc
import           GHC.Unit.Finder as Ghc
import           GHC.Unit.Module.Name as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Data.FastString as Ghc
import           GHC.Hs.Utils as Ghc
import           GHC.Types.Unique.Set as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Hs.Binds as Ghc
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
import           GHC.Hs.Expr as Ghc
import           GHC.Types.PkgQual as Ghc
import           GHC.Tc.Types.Origin as Ghc

#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Hs.Extension as Ghc
import           Language.Haskell.Syntax as Ghc
import           GHC.Tc.Types as Ghc
import qualified GHC.Tc.Plugin as Plugin
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
import           GHC.Hs.Expr as Ghc
import           GHC.Tc.Types.Origin as Ghc

#endif

liftedRepName :: Ghc.Name
liftedRepName = Ghc.getName Ghc.liftedRepTyCon

mkLexicalFastString :: Ghc.FastString -> Ghc.LexicalFastString
fromLexicalFastString :: Ghc.LexicalFastString -> Ghc.FastString
mkLexicalFastString = Ghc.LexicalFastString
fromLexicalFastString (Ghc.LexicalFastString fs) = fs

collectHsBindBinders' :: Ghc.HsBindLR Ghc.GhcRn idR -> [Ghc.Name]
collectHsBindBinders' = Ghc.collectHsBindBinders Ghc.CollNoDictBinders

collectPatBinders' :: Ghc.LPat Ghc.GhcRn -> [Ghc.Name]
collectPatBinders' = Ghc.collectPatBinders Ghc.CollNoDictBinders

mkWildValBinder' :: Ghc.Type -> Ghc.Id
mkWildValBinder' = Ghc.mkWildValBinder Ghc.oneDataConTy

pprTypeForUser' :: Ghc.Type -> Ghc.SDoc
#if MIN_VERSION_ghc(9,4,0)
pprTypeForUser' = Ghc.pprSigmaType
#else
pprTypeForUser' = Ghc.pprTypeForUser
#endif

showSDocOneLine' :: Ghc.SDoc -> String
showSDocOneLine' = Ghc.showSDocOneLine Ghc.defaultSDocContext

findImportedModule' :: Ghc.ModuleName -> Ghc.TcPluginM Ghc.FindResult
#if MIN_VERSION_ghc(9,4,0)
findImportedModule' modName = Plugin.findImportedModule modName Ghc.NoPkgQual
#else
findImportedModule' modName = Plugin.findImportedModule modName Nothing
#endif

findPluginModule' :: Ghc.ModuleName -> Ghc.TcM Ghc.FindResult
#if MIN_VERSION_ghc(9,4,0)
findPluginModule' modName =
  Ghc.runTcPluginM $ Plugin.findImportedModule modName Ghc.NoPkgQual
#else
findPluginModule' modName = do
  hscEnv <- Ghc.getTopEnv
  liftIO $ Ghc.findPluginModule hscEnv modName
#endif

#if MIN_VERSION_ghc(9,4,0)
type LetToken =
  Ghc.LHsToken "let" Ghc.GhcRn
type InToken =
  Ghc.LHsToken "in" Ghc.GhcRn
#else
type LetToken = ()
type InToken = ()
#endif

pattern HsLet'
  :: Ghc.XLet Ghc.GhcRn
  -> LetToken
  -> Ghc.Located (Ghc.HsLocalBinds Ghc.GhcRn)
  -> InToken
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.HsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,4,0)
pattern HsLet' x letToken lbinds inToken expr <-
  Ghc.HsLet x letToken (Ghc.L Ghc.noSrcSpan -> lbinds) inToken expr
  where
    HsLet' x letToken (Ghc.L _ binds) inToken expr =
      Ghc.HsLet x letToken binds inToken expr
#elif MIN_VERSION_ghc(9,2,0)
pattern HsLet' x letToken lbinds inToken expr <-
  Ghc.HsLet (pure . pure -> (letToken, (inToken, x))) (Ghc.L Ghc.noSrcSpan -> lbinds) expr
  where
    HsLet' x () (Ghc.L _ binds) () expr = Ghc.HsLet x binds expr
#else
pattern HsLet' x letToken lbinds inToken expr <-
  Ghc.HsLet (pure . pure -> (letToken, (inToken, x))) lbinds expr
  where
    HsLet' x _ lbinds _ expr = Ghc.HsLet x lbinds expr
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

pattern OverLit'
  :: Ghc.OverLitVal
  -> Ghc.HsOverLit Ghc.GhcRn
pattern OverLit' lit
#if MIN_VERSION_ghc(9,4,0)
  <- Ghc.OverLit _ lit
#else
  <- Ghc.OverLit _ lit _
#endif

pattern CDictCan'
  :: Ghc.CtEvidence
  -> Ghc.Class
  -> [Ghc.Xi]
  -> Ghc.Ct
pattern CDictCan' diEv diCls diTys
#if MIN_VERSION_ghc(9,8,0)
  <- Ghc.CDictCan (Ghc.DictCt diEv diCls diTys _)
#else
  <- Ghc.CDictCan { Ghc.cc_ev = diEv, Ghc.cc_class = diCls, Ghc.cc_tyargs = diTys }
#endif

