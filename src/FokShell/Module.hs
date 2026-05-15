{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module FokShell.Module where
import Lib.Keys (KeyEvent)
import Control.Arrow (Arrow(first, second))
import Data.List (singleton)
import Data.Bool (bool)
import Data.Maybe
import Data.Proxy
import Data.Typeable

class Module' a proc where
  -- | hook called upon shell's startup
  initHook'    :: a -> proc -> IO (a, proc)
  -- | hook called upon shell exit
  exitHook'    :: a -> proc -> IO (a, proc)
  -- | hook called in eg. haltHook, used to restore the default state of the Module
  resetHook'   :: a -> proc -> IO (a, proc)
  -- | hook called before default processing of keyevents
  preHook'     :: a -> proc -> KeyEvent -> IO (Bool, (a, proc))
  -- | hook called after default processing of keyevents
  postHook'    :: a -> proc -> KeyEvent -> IO (Bool, (a, proc))

data Module p where
  Module :: (Module' a p,Typeable a) => a -> Module p

initHook :: Module p -> p -> IO (Module p, p)
initHook (Module a) p = first Module <$> initHook' a p
exitHook :: Module p -> p -> IO (Module p, p)
exitHook (Module a) p = first Module <$> exitHook' a p

resetHook :: Module p -> p -> IO (Module p, p)
resetHook (Module a) p = first Module <$> resetHook' a p

preHook :: Module p -> p -> KeyEvent -> IO (Bool, (Module p, p))
preHook (Module a) p e = second (first Module) <$> preHook' a p e
postHook :: Module p -> p -> KeyEvent -> IO (Bool, (Module p, p))
postHook (Module a) p e = second (first Module) <$> postHook' a p e

class ModuleContainer p where
  getModules :: p -> [Module p]
  setModules :: p -> [Module p] -> p

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

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt i y (x:xs) = x:replaceAt (i-1) y xs

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

withProxy :: forall i p. Typeable i => Proxy i -> Module p -> Maybe i
withProxy _ (Module a) = cast a
requestModule :: forall a p. (Module' a p,Typeable a) => Proxy a -> [Module p] -> [a]
requestModule p xs = fmap fromJust $ filter isJust $ fmap (withProxy p) xs

modifyModule :: forall a p. (Module' a p,Typeable a) => Proxy a -> [Module p] -> (a -> a) -> [Module p]
modifyModule _ [] _ = []
modifyModule p (x:xs) f = fapply:modifyModule p xs f
  where
    fapply = case withProxy p x of
        Just x' -> Module $ f x'
        Nothing -> x
