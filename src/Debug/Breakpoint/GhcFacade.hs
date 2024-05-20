{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import           GHC.Unit.Types as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Data.FastString as Ghc
import           GHC.Hs.Utils as Ghc
import           GHC.Types.Unique.Set as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Hs.Binds as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Types.Basic as Ghc
import           GHC.Types.Name.Env as Ghc
import           GHC.Builtin.Names as Ghc
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
import           GHC.Unit.Types as Ghc
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
import           GHC.Builtin.Names as Ghc
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
pprTypeForUser' = Ghc.pprSigmaType

showSDocOneLine' :: Ghc.SDoc -> String
showSDocOneLine' = Ghc.showSDocOneLine Ghc.defaultSDocContext

findImportedModule' :: Ghc.ModuleName -> Ghc.TcPluginM Module
findImportedModule' modName =
  Plugin.findImportedModule modName Ghc.NoPkgQual >>= \case
    Found _ m -> pure m
    _ -> fail "Could not find module!"

findPluginModule' :: Ghc.ModuleName -> Ghc.TcM Ghc.FindResult
#if MIN_VERSION_ghc(9,4,0)
findPluginModule' modName =
  Ghc.runTcPluginM $ Plugin.findImportedModule modName Ghc.NoPkgQual
#else
findPluginModule' modName = do
  hscEnv <- Ghc.getTopEnv
  liftIO $ Ghc.findPluginModule hscEnv modName
#endif

type LetToken =
#if MIN_VERSION_ghc(9,10,0)
  ()
#else
  Ghc.LHsToken "let" Ghc.GhcRn
#endif
type InToken =
#if MIN_VERSION_ghc(9,10,0)
  ()
#else
  Ghc.LHsToken "in" Ghc.GhcRn
#endif

pattern HsLet'
  :: Ghc.XLet Ghc.GhcRn
  -> LetToken
  -> Ghc.HsLocalBinds Ghc.GhcRn
  -> InToken
  -> Ghc.LHsExpr Ghc.GhcRn
  -> Ghc.HsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
hsLetShim :: x -> (x, (), ())
hsLetShim x = (x, (), ())
pattern HsLet' x letToken lbinds inToken expr <-
  Ghc.HsLet (hsLetShim -> (x, letToken, inToken)) lbinds expr
  where
    HsLet' x () binds () expr =
      Ghc.HsLet x binds expr
#else
pattern HsLet' x letToken lbinds inToken expr <-
  Ghc.HsLet x letToken lbinds inToken expr
  where
    HsLet' x letToken binds inToken expr =
      Ghc.HsLet x letToken binds inToken expr
#endif

pattern OverLit'
  :: Ghc.OverLitVal
  -> Ghc.HsOverLit Ghc.GhcRn
pattern OverLit' lit
  <- Ghc.OverLit _ lit

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

