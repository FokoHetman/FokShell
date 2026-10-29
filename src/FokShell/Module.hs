{-# LANGUAGE GADTs, ScopedTypeVariables, AllowAmbiguousTypes, FunctionalDependencies #-}
module FokShell.Module where
import Lib.Keys (KeyEvent)
import Data.Bool (bool)
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Control.Monad.Error.Class (MonadError (catchError))

class ModuleError e where
  moduleNotFoundError :: e

class ModuleContainer m where
  getModules   :: m [Module m]

class Configurable runtime conf | conf -> runtime where
  configure :: conf -> IO runtime

data ModuleConfig m where ModuleConfig :: (Configurable runtime conf, Module' runtime m, Typeable runtime) => conf -> ModuleConfig m

configureModule :: ModuleConfig m -> IO (Module m)
configureModule (ModuleConfig c) = Module <$> configure c

class Monad m => Module' a m where
  -- | hook called upon shell's startup
  initHook    :: a -> m ()
  -- | hook called upon shell exit
  exitHook    :: a -> m ()
  -- | hook called in eg. haltHook, used to restore the default state of the Module
  resetHook   :: a -> m Bool
  -- | hook called before default processing of keyevents
  preHook     :: a -> KeyEvent -> m Bool
  -- | hook called after default processing of keyevents
  postHook    :: a -> KeyEvent -> m Bool

data Module p where
  Module :: (Module' a p,Typeable a) => a -> Module p

instance Monad m => Module' (Module m) m where
  initHook (Module a) = initHook a
  exitHook (Module a) = exitHook a
  resetHook (Module a) = resetHook a
  preHook (Module a) = preHook a
  postHook (Module a) = postHook a
{- This is such a nice system I'm sad I'm replacing it
class ModuleContainer p where
  getModules :: p -> [Module p]
  setModules :: p -> [Module p] -> p

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt i y (x:xs) = x:replaceAt (i-1) y xs

chainHook :: forall p. ModuleContainer p => p -> (Module p -> p -> IO (Module p, p)) -> IO  p
chainHook p hook = go p $ length $ getModules p
  where
    go :: p -> Int -> IO p
    go p 0 = pure p
    go p x = do
      let ms = getModules p
          i = (length ms - x)
      case splitAt i ms of
        (_,[]) -> pure p
        (_,m:_) -> do
          (m', p') <- hook m p
          let ms' = replaceAt i m' $ getModules p'
          let p'' = setModules p' ms'
          go p'' (x-1)

chainEventHook :: forall p. ModuleContainer p => p -> (Module p -> p -> KeyEvent -> IO (Bool, (Module p, p))) -> KeyEvent -> IO (Bool, p)
chainEventHook p hook event = go p $ length $ getModules p
  where
    go :: p -> Int -> IO (Bool, p)
    go p 0 = pure (True, p)
    go p x = do
      let ms = getModules p
          i = (length ms - x)
      case splitAt i ms of
        (_,[]) -> pure (True,p)
        (_,m:_) -> do
          (b,(m', p')) <- hook m p event
          let ms' = replaceAt i m' $ getModules p'
          let p'' = setModules p' ms'
          bool (pure (b,p'')) (go p'' (x-1)) b
-}

chainHook :: forall e m. (Monad m, ModuleError e, MonadError e m, ModuleContainer m) => (Module m -> m ()) -> m ()
chainHook hook = do
  modules <- getModules
  chainHookL modules hook

chainBoolHook :: forall e m. (Monad m, ModuleError e, MonadError e m, ModuleContainer m) => (Module m -> m Bool) -> m Bool
chainBoolHook hook = do
  modules <- getModules
  chainBoolHookL modules hook


chainEventHook :: forall e m. (Monad m, ModuleError e, MonadError e m, ModuleContainer m) => (Module m -> KeyEvent -> m Bool) -> KeyEvent -> m Bool
chainEventHook hook event = do
  modules <- getModules
  chainEventHookL modules hook event


chainHookL :: forall e m. (Monad m, ModuleError e, MonadError e m) => [Module m] -> (Module m -> m ()) -> m ()
chainHookL [] _ = pure ()
chainHookL (m:ms) hook = (hook m `catchError` const (pure ())) >> chainHookL ms hook

chainEventHookL :: forall e m. (Monad m, ModuleError e, MonadError e m) => [Module m] -> (Module m -> KeyEvent -> m Bool) -> KeyEvent -> m Bool
chainEventHookL [] _ _ = pure True
chainEventHookL (m:ms) hook e = do
  b <- hook m e `catchError` const (pure True)
  bool (pure False) (chainEventHookL ms hook e) b

chainBoolHookL :: forall e m. (Monad m, ModuleError e, MonadError e m) => [Module m] -> (Module m -> m Bool) -> m Bool
chainBoolHookL [] _ = pure True
chainBoolHookL (m:ms) hook = do
  b <- hook m `catchError` const (pure True)
  bool (pure False) (chainBoolHookL ms hook) b




withProxy :: forall i p. Typeable i => Proxy i -> Module p -> Maybe i
withProxy _ (Module a) = cast a

withType :: forall i p. Typeable i => Module p -> Maybe i
withType (Module a) = cast a

requestModule :: forall a p. (Module' a p,Typeable a) => [Module p] -> [a]
requestModule xs = fmap fromJust $ filter isJust $ fmap withType xs
--requestModule :: forall a p. (Module' a p,Typeable a) => Proxy a -> [Module p] -> [TVar a]
--requestModule p xs = fmap fromJust $ filter isJust $ fmap (withProxy p) xs
{-
modifyModule :: forall a p. (Module' a p,Typeable a) => Proxy a -> [Module p] -> (a -> a) -> [Module p]
modifyModule _ [] _ = []
modifyModule p (x:xs) f = fapply:modifyModule p xs f
  where
    fapply = case withProxy p x of
        Just x' -> Module $ f x'
        Nothing -> x-}
