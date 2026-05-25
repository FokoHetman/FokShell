{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module FokShell.Module where
import Lib.Keys (KeyEvent)
import Data.Bool (bool)
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Control.Concurrent.STM

class ModuleContainer p where
  getModules   :: p -> [Module p]

class Module' a proc where
  -- | hook called upon shell's startup
  initHook'    :: TVar a -> TVar proc -> IO ()
  -- | hook called upon shell exit
  exitHook'    :: TVar a -> TVar proc -> IO ()
  -- | hook called in eg. haltHook, used to restore the default state of the Module
  resetHook'   :: TVar a -> TVar proc -> IO ()
  -- | hook called before default processing of keyevents
  preHook'     :: TVar a -> TVar proc -> KeyEvent -> IO Bool
  -- | hook called after default processing of keyevents
  postHook'    :: TVar a -> TVar proc -> KeyEvent -> IO Bool

data Module p where
  Module :: (Module' a p,Typeable a) => TVar a -> Module p


module' :: (Typeable a, Module' a p) => a -> STM (Module p)
module' m = Module <$> newTVar m
initHook, exitHook, resetHook :: (Module p) -> TVar p -> IO ()
initHook (Module a) p = initHook' a p
exitHook (Module a) p = exitHook' a p
resetHook (Module a) p = resetHook' a p

preHook, postHook :: Module p -> TVar p -> KeyEvent -> IO Bool
preHook (Module a) p e = preHook' a p e
postHook (Module a) p e = postHook' a p e
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

chainHook :: forall p. ModuleContainer p => TVar p -> (Module p -> TVar p -> IO ()) -> IO ()
chainHook pvar hook = do
  modules <- getModules <$> atomically (readTVar pvar)
  chainHookL modules pvar hook

chainEventHook :: forall p. ModuleContainer p => TVar p -> (Module p -> TVar p -> KeyEvent -> IO Bool) -> KeyEvent -> IO Bool
chainEventHook pvar hook event = do
  modules <- getModules <$> atomically (readTVar pvar)
  chainEventHookL modules pvar hook event


chainHookL :: forall p. [Module p] -> TVar p -> (Module p -> TVar p -> IO ()) -> IO ()
chainHookL [] _ _ = pure ()
chainHookL (m:ms) p hook = hook m p >> chainHookL ms p hook

chainEventHookL :: forall p. [Module p] -> TVar p -> (Module p -> TVar p -> KeyEvent -> IO Bool) -> KeyEvent -> IO Bool
chainEventHookL [] _ _ _ = pure True
chainEventHookL (m:ms) p hook e = do
  b <- hook m p e
  bool (pure False) (chainEventHookL ms p hook e) b




withProxy :: forall i p. Typeable i => Proxy i -> Module p -> Maybe i
withProxy _ (Module a) = cast a

-- todo: convert to this
withType :: forall i p. Typeable i => Module p -> Maybe i
withType (Module a) = cast a

requestModule :: forall a p. (Module' a p,Typeable a) => [Module p] -> [TVar a]
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
