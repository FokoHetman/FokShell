module Lib.Format where

import qualified Data.Text as T
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.List (singleton)
import qualified Data.Text.IO as T
import System.Environment (getEnv)
import Control.Monad (filterM)
import System.Directory (getPermissions, Permissions (executable), doesDirectoryExist, canonicalizePath, getDirectoryContents)
import System.FilePath.Posix ((</>))

moveCursor :: Direction -> Int -> IO ()
moveCursor _ 0 = pure ()
moveCursor DLeft i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "D"
moveCursor DRight i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "C"
moveCursor Up i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "A"
moveCursor Down i = putStrf $ T.pack $ "\ESC[" ++ show i ++ "B"

data Direction = Up | Down | DRight | DLeft
    deriving (Show,Eq)

-- DISPLAY FUNCTIONS
eputStrf :: IO T.Text -> IO ()
eputStrf t = t >>= \x -> T.putStr x <> hFlush stdout

putStrf :: T.Text -> IO ()
putStrf t = putStr (T.unpack t) <> hFlush stdout

differ :: T.Text -> T.Text -> T.Text
differ t1 t2
  | T.null t1 = t2
  | T.null t2 = t1
  | T.head t1 == T.head t2 = differ (T.tail t1) (T.tail t2)
  | T.head t1 /= T.head t2 = (T.pack . singleton $ T.head t2) <> differ (T.tail t1) (T.tail t2)


getDirsInPath :: IO [FilePath]
getDirsInPath = filterM doesDirectoryExist . fmap T.unpack . T.split (==':') . T.pack =<< getEnv "PATH"

executablesInDir :: FilePath -> IO [FilePath]
executablesInDir t = getDirectoryContents t >>= mapM (pure . (t</>)) >>= mapM canonicalizePath >>= filterM (fmap executable . getPermissions . (t</>)) --filterM (fmap executable . getPermissions . (t</>))


