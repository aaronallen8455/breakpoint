{-# LANGUAGE DataKinds #-}
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
  , mkWildValBinder'
  , pprTypeForUser'
  , showSDocOneLine'
  , findImportedModule'
  , findPluginModule'
  , pattern HsLet'
  , pattern LetStmt'
  , pattern ExplicitList'
  , pattern BindStmt'
  , pattern OverLit'
  ) where

#if MIN_VERSION_ghc(9,4,0)
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
import           GHC.Hs.Expr as Ghc
import           GHC.Types.PkgQual as Ghc

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
import           GHC.Hs.Expr as Ghc

#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Plugins as Ghc hiding (TcPlugin)
import           GHC.Driver.Finder as Ghc
import           GHC.Hs.Extension as Ghc
import           GHC.Tc.Types as Ghc
import qualified GHC.Tc.Plugin as Plugin
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
import           GHC.Driver.Session as Ghc
import           GHC.Hs.Expr as Ghc
import           GHC.Hs.Pat as Ghc
import           GHC.Hs.Decls as Ghc
import           GHC.Hs.Lit as Ghc

#elif MIN_VERSION_ghc(8,10,0)
import           GHC.Hs.Expr as Ghc
import           GHC.Hs.Extension as Ghc
import           GHC.Hs.Binds as Ghc
import           GHC.Hs.Lit as Ghc
import           SrcLoc as Ghc
import           GHC.Hs.Utils as Ghc
import           Name as Ghc
import           GHC.Hs.Pat as Ghc
import           FastString as Ghc
import           TysWiredIn as Ghc
import           InstEnv as Ghc
import           TcEvidence as Ghc
import           TyCoRep as Ghc
import           MkCore as Ghc
import           PprTyThing as Ghc
import           Outputable as Ghc
import           TcRnTypes as Ghc
import           Type as Ghc
import           TcType as Ghc
import           Id as Ghc
import           Class as Ghc
import           Constraint as Ghc
import           Module as Ghc
import           HscTypes as Ghc
import           TyCon as Ghc
import           Bag as Ghc
import           BasicTypes as Ghc
import           UniqSet as Ghc
import           NameEnv as Ghc
import           IfaceEnv as Ghc
import           Finder as Ghc hiding (findImportedModule)
import           TcPluginM as Ghc hiding (lookupOrig, getTopEnv, getEnvs, newUnique)
import           GHC.Hs.Decls as Ghc
import           TcRnMonad as Ghc
import           Plugins as Ghc hiding (TcPlugin)
import           DynFlags as Ghc
import qualified TcPluginM as Plugin
#endif

liftedRepName :: Ghc.Name
#if MIN_VERSION_ghc(9,2,0)
liftedRepName = Ghc.getName Ghc.liftedRepTyCon
#else
liftedRepName = Ghc.getName Ghc.liftedRepDataCon
#endif

#if MIN_VERSION_ghc(9,2,0)
type LexicalFastString' = Ghc.LexicalFastString
#else
type LexicalFastString' = Ghc.FastString
#endif

mkLexicalFastString :: Ghc.FastString -> LexicalFastString'
fromLexicalFastString :: LexicalFastString' -> Ghc.FastString
#if MIN_VERSION_ghc(9,2,0)
mkLexicalFastString = Ghc.LexicalFastString
fromLexicalFastString (Ghc.LexicalFastString fs) = fs
#else
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

mkWildValBinder' :: Ghc.Type -> Ghc.Id
#if MIN_VERSION_ghc(9,0,0)
mkWildValBinder' = Ghc.mkWildValBinder Ghc.oneDataConTy
#else
mkWildValBinder' = Ghc.mkWildValBinder
#endif

pprTypeForUser' :: Ghc.Type -> Ghc.SDoc
#if MIN_VERSION_ghc(9,4,0)
pprTypeForUser' = Ghc.pprSigmaType
#else
pprTypeForUser' = Ghc.pprTypeForUser
#endif

showSDocOneLine' :: Ghc.SDoc -> String
showSDocOneLine' =
#if MIN_VERSION_ghc(9,2,0)
  Ghc.showSDocOneLine Ghc.defaultSDocContext
#elif MIN_VERSION_ghc(9,0,0)
  Ghc.showSDocOneLine
    $ Ghc.initDefaultSDocContext Ghc.unsafeGlobalDynFlags
#else
  Ghc.showSDocOneLine Ghc.unsafeGlobalDynFlags
#endif

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

#if MIN_VERSION_ghc(9,0,0)
mkSyntaxExprs :: x -> (SyntaxExpr Ghc.GhcRn, SyntaxExpr Ghc.GhcRn, x)
mkSyntaxExprs x = (Ghc.noSyntaxExpr, Ghc.noSyntaxExpr, x)
#endif

pattern BindStmt'
  :: Ghc.XBindStmt Ghc.GhcRn Ghc.GhcRn body
  -> Ghc.LPat Ghc.GhcRn
  -> body
  -> SyntaxExpr Ghc.GhcRn
  -> SyntaxExpr Ghc.GhcRn
  -> Ghc.Stmt Ghc.GhcRn body
#if MIN_VERSION_ghc(9,0,0)
pattern BindStmt' x pat body expr1 expr2
    <- Ghc.BindStmt x pat (mkSyntaxExprs -> (expr1, expr2, body))
  where
    BindStmt' x pat body _ _ = Ghc.BindStmt x pat body
#else
pattern BindStmt' x pat body bindExpr failExpr = Ghc.BindStmt x pat body bindExpr failExpr
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
