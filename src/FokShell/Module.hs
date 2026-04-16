{-# LANGUAGE GADTs #-}
module FokShell.Module where
import Lib.Keys (KeyEvent)
import Control.Arrow (Arrow(first, second))
import Data.List (singleton)
import Data.Bool (bool)

class Module' a proc where
  initHook'    :: a -> proc -> IO (a, proc)
  preHook'     :: a -> proc -> KeyEvent -> IO (Bool, (a, proc))
  postHook'    :: a -> proc -> IO (a, proc)

data Module p where
  Module :: Module' a p => a -> Module p

initHook :: Module p -> p -> IO (Module p, p)
initHook (Module a) p = first Module <$> initHook' a p

preHook :: Module p -> p -> KeyEvent -> IO (Bool, (Module p, p))
preHook (Module a) p e = second (first Module) <$> preHook' a p e
postHook :: Module p -> p -> IO (Module p, p)
postHook (Module a) p = first Module <$> postHook' a p

chainHook :: [Module p] -> p -> (Module p -> p -> IO (Module p, p)) -> IO ([Module p], p)
chainHook [] p _ = pure ([], p)
chainHook [x] p hook = first singleton <$> hook x p
chainHook (x:xs) p hook = do
  (x',p') <- hook x p
  (xs',p'') <- chainHook xs p' hook
  pure (x':xs',p'')

chainEventHook :: [Module p] -> p -> (Module p -> p -> KeyEvent -> IO (Bool, (Module p, p))) -> KeyEvent -> IO (Bool, ([Module p], p))
chainEventHook [] p _ _ = pure (True, ([], p))
chainEventHook [x] p hook event = second (first singleton) <$> hook x p event
chainEventHook (x:xs) p hook event = do
  (b, (x', p')) <- hook x p event
  bool
    (pure (b, (x':xs, p')))
    (do
      (b',(xs',p'')) <- chainEventHook xs p' hook event
      (pure (b && b', (x':xs', p'')))
    )
    b
