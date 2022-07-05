module Debug.BreakPoint
  ( plugin
  , traceVars
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Data
import           Data.Functor.Identity
import qualified Data.Generics as Syb
import qualified Data.Map as M

import qualified Debug.BreakPoint.GhcFacade as Ghc

traceVars :: [String]
traceVars = []

recursion :: Data a => VarMap -> a -> EnvState a
recursion resultVarMap a =
  maybe (gmapM (recursion resultVarMap) a) pure
    =<< transform resultVarMap a

newtype T a = T (a -> EnvState (Maybe a))

transform :: Data a => VarMap -> a -> EnvState (Maybe a)
transform resultVarMap a = fmap join . runMaybeT $
    wrap (hsVarCase resultVarMap)
  where
    wrap f = MaybeT $ do
      case gcast (T f) of
        Nothing -> pure Nothing
        Just (T f') -> Just <$> f' a

hsVarCase :: VarMap
          -> Ghc.HsExpr Ghc.GhcRn
          -> EnvState (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsVarCase resultVarMap x@(Ghc.HsVar _ (Ghc.L srcSpanAnn name)) = do
  matchesTarget <- gets $ (== name) . targetName
  if matchesTarget
     then do
       let srcSpan = OrderedSrcSpan $ Ghc.locA srcSpanAnn
       modify' (overMap $ M.insert srcSpan [])
       nameShow <- gets showName
       -- knot tying
       let names = Ghc.nlHsVar <$> (resultVarMap M.! srcSpan)

           expr = Ghc.ExplicitList Ghc.noExtField $
             Ghc.nlHsApp (Ghc.nlHsVar nameShow) <$> names

       pure (Just expr)
     else pure Nothing
hsVarCase _ _ = pure Nothing

type EnvState a = State Env a

newtype OrderedSrcSpan = OrderedSrcSpan Ghc.SrcSpan
  deriving Eq

instance Ord OrderedSrcSpan where
  compare = coerce Ghc.leftmost_smallest

type VarMap = M.Map OrderedSrcSpan [Ghc.Name]

data Env = MkEnv
  { varMap :: !VarMap
  , targetName :: !Ghc.Name
  , showName :: !Ghc.Name
  }

overMap :: (VarMap -> VarMap) -> Env -> Env
overMap f env = env { varMap = f $ varMap env }

-- Need to have access to Names
-- Need to build the var Map
-- Is it okay to use a state monad? Will it be a problem for knot tying?

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const renameAction
  }

-- TODO
-- Instead of knot tying, could accumulate a set of var names. That is probably
-- a bit better actually. It can't be part of the state though since it should
-- not be propagated between different scopes. Can use UniqSet for the names.
-- Then there is no need for state monad at all, can just be a reader and use
-- local to add names to the set for particular cases.

renameAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renameAction gblEnv group = do
  hscEnv <- Ghc.getTopEnv
  Ghc.Found _ breakPointMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug.BreakPoint") Nothing
  -- TODO this could fail if base if not a dependency?
  Ghc.Found _ preludeMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Prelude") (Just $ Ghc.fsLit "base")

  -- TODO cache these lookups somehow?
  traceVarsName <- Ghc.lookupOrig breakPointMod (Ghc.mkVarOcc "traceVars")
  showName <- Ghc.lookupOrig preludeMod (Ghc.mkVarOcc "show")

  let (group', resultEnv) =
        runState (recursion (varMap resultEnv) group)
          MkEnv { varMap = mempty
                , targetName = traceVarsName
                , showName = showName
                }
  pure (gblEnv, group')

-- Top down, left to right
-- everywhereMI
--   :: Monad m
--   => Syb.GenericQ Bool
--   -> m ()
--   -> (forall a. Data a => Int -> a -> m a)
--   -> Syb.GenericM m
-- everywhereMI incQ afterEffect f = go 0 where
--   go !i x = do
--     let i' = if incQ x
--                 then i + 1
--                 else i
--     r <- Syb.gmapM (go i') =<< f i' x
--     -- pop the context stack here?
--     afterEffect
--     pure r

-- Could increment the scope level on any HsExpr. The issue is that things will
-- have the same scope level but not be visible to each other. There needs to
-- be some way to remove bindings from the context when the scope level gets
-- popped.
-- Perhaps the context could be a list of maps of bindings and after a branch
-- is processed, that list gets popped so that those bindings are no longer
-- in play when the sibling gets processed. This will require use of state monad.
--
-- Current strat is to push a new map onto the bindings stack when a new HsExpr
-- is entered and the pop it off the stack after that HsExpr has been descended
-- into. If the special function is encountered then all bindings in the context
-- regardless of the stack delimination are used as the in scope bindings.
-- If I'm lucky then simply pushing on the stack for HsExprs is enough to handle
-- scoping correctly.
