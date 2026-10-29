{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module FokShell.Module.Input where
import Control.Concurrent.STM (TVar, atomically, newTVar, writeTVar, readTVar, modifyTVar)
import qualified Data.Text as T
import FokShell.Module
import FokShell.Types (ShellAction)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib.Keys
import Lib.Format
import Control.Monad (when, unless)

import Data.Functor
import Data.Maybe (fromMaybe)
import Lib.Primitive
import qualified Data.Text.IO as T
import Debug.Trace (traceIO)

data InputConfig = InputConfig {}
instance Def InputConfig where
  def = InputConfig
data Input = Input
  { input :: TVar T.Text
  , cursorLocation :: TVar Int
  }

redrawFromCursor :: Input -> Maybe Int -> ShellAction ()
redrawFromCursor inp loc = liftIO $ do
  (input, cursorLocation) <- atomically $ (,) <$> readTVar inp.input <*> readTVar inp.cursorLocation
  let 
    erase = T.pack "\ESC[0K"
    lefts = T.reverse $ T.take (fromMaybe cursorLocation loc) (T.reverse $ input)
    cursorCode = if T.length lefts > 0 then T.concat ["\ESC[", T.pack $ show $ T.length lefts, "D"] else T.empty
  putStrf $ T.concat [erase, lefts, cursorCode]

writeInput :: Input -> T.Text -> ShellAction ()
writeInput inp t = liftIO . atomically $ do
  input <- readTVar inp.input
  cursorLocation <- readTVar inp.cursorLocation
  let (left, right) = T.splitAt (T.length input - cursorLocation) input
  writeTVar inp.input $ left <> t <> right

deleteInput :: Input -> Direction -> Int -> ShellAction ()
deleteInput inp DLeft i = unless (i == 0) . liftIO $ do
  moveby <- atomically $ do
    input <- readTVar inp.input
    cursorLocation <- readTVar inp.cursorLocation
    let leftLen = T.length input - cursorLocation
    let (left, right) = T.splitAt leftLen input
    writeTVar inp.input $ T.dropEnd i left <> right
    pure $ min i leftLen
  unless (moveby == 0) $ moveCursor DLeft moveby
deleteInput inp DRight i = unless (i == 0) . liftIO $ do
  moveby <- atomically $ do
    input <- readTVar inp.input
    cursorLocation <- readTVar inp.cursorLocation
    let (left, right) = T.splitAt (T.length input - cursorLocation) input
    writeTVar inp.input $ left <> T.drop i right
    modifyTVar inp.cursorLocation $ \loc -> max 0 (loc - i)
    pure $ min i cursorLocation 
  unless (moveby == 0) $ moveCursor DRight moveby
deleteInput _ _ _ = error "invalid direction provided to deleteInput"
moveInputCursor :: Input -> Direction -> Int -> ShellAction ()
moveInputCursor inp DLeft by = unless (by == 0) . liftIO $ do
  proceed <- atomically $ do
    input <- readTVar inp.input
    current <- readTVar inp.cursorLocation
    let check = T.length input >= by + current
    when check $ writeTVar inp.cursorLocation $ current + by
    pure check
  when proceed $ moveCursor DLeft by
moveInputCursor inp DRight by = liftIO $ do
  proceed <- atomically $ do
    current <- readTVar inp.cursorLocation
    let check = current - by >= 0
    when check $ writeTVar inp.cursorLocation $ current - by
    pure check
  when proceed $ moveCursor DRight by
moveInputCursor _ _ _ = error "invalid direction provided to moveInputCursor"

instance Configurable Input InputConfig where
  configure _ = atomically $ do
    input <- newTVar ""
    cursorLocation <- newTVar 0
    pure Input{..}

instance Module' Input ShellAction where
  initHook _ = pure ()
  exitHook _ = pure ()
  resetHook i = liftIO . atomically $ do
    writeTVar i.input ""
    writeTVar i.cursorLocation 0
    pure True
  preHook i = \case
    (KeyModifiers 0, Character c) -> do
      writeInput i c
      liftIO $ T.putStr c
      redrawFromCursor i Nothing
      pure False
    (KeyModifiers 0, Arrow DLeft) -> moveInputCursor i DLeft 1 $> False
    (KeyModifiers 0, Arrow DRight) -> moveInputCursor i DRight 1 $> False
    (KeyModifiers 0, Backspace) -> deleteInput i DLeft 1 >> redrawFromCursor i Nothing $> False
    _ -> pure True
  postHook _ _ = pure True
