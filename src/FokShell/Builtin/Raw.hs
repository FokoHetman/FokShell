{-# LANGUAGE LambdaCase #-}
module FokShell.Builtin.Raw where
import FokShell.Types
import System.Process
import Data.ByteString qualified as BS
import Data.Text qualified as T
import GHC.IO.Handle (Handle, hClose)

import GHC.IO.Exception (IOException)
import System.Posix
import Control.Monad.RWS (MonadReader(ask), MonadIO (liftIO))
import Control.Concurrent.Async (withAsync, link, cancel)
import Control.Exception (finally, throwIO, try)
import System.IO.Error (isResourceVanishedError)
import Control.Monad (void, unless)
import Data.Maybe (fromMaybe, isJust)

rawProcess :: T.Text -> Process BS.ByteString BS.ByteString
rawProcess name args source sink = do
  let cp = (proc (T.unpack name) (map T.unpack args))
        { std_in      = fromMaybe CreatePipe $ source.rawOverride
        , std_out     = fromMaybe CreatePipe $ sink.rawOverride
        , std_err     = Inherit
        , create_group = True
        }
  env <- ask
  liftIO $ withCreateProcess cp $ \mIn mOut _ ph -> do
    getPid ph >>= \case
      Just pid -> do
        pgid <- getProcessGroupIDOf pid
        setTerminalProcessGroupID env.ttyFd pgid
      Nothing -> pure ()
    withAsync (case mIn of
      Just hin -> unless (isJust source.rawOverride) $ pumpInput source hin
      Nothing -> pure ()
      ) $ \input ->
      withAsync (case mOut of
        Just hout -> unless (isJust sink.rawOverride) $ pumpOutput hout sink
        Nothing -> pure ()
      ) $ \output -> do
      link input
      link output
      exitCode <- waitForProcess ph
      cancel input
      cancel output
      setTerminalProcessGroupID env.ttyFd env.pgid
      pure exitCode
pumpInput :: Source BS.ByteString -> Handle -> IO ()
pumpInput source h = loop `finally ` closeQuietly h
  where
    loop = do
      next <- pull source
      case next of
        Nothing -> pure ()
        Just chunk -> do
          result <- (try (BS.hPut h chunk) :: IO (Either IOException ()))
          case result of
            Right () -> loop
            Left e
              | isResourceVanishedError e -> pure ()
              | otherwise -> throwIO e
pumpOutput :: Handle -> Sink BS.ByteString -> IO ()
pumpOutput h sink = loop
  where
    loop = do
      chunk <- BS.hGetSome h 32768
      if BS.null chunk
        then finish sink
        else do
          push sink chunk
          loop
closeQuietly :: Handle -> IO ()
closeQuietly h = void (try (hClose h :: IO ()) :: IO (Either IOError ()))
