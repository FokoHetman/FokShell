{-# LANGUAGE DataKinds, OverloadedStrings, LambdaCase #-}
module FokShell.Builtin.Core where
import FokShell.Types
import Data.Void (Void)
import System.Posix (FileMode, FileOffset, EpochTime, getFileStatus, fileMode, isDirectory, statusChangeTime, accessTime, modificationTime, fileSize, fileGroup, getGroupEntryForID, groupName, userName, getUserEntryForID, fileOwner, isSocket, isBlockDevice, getSymbolicLinkStatus)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad.IO.Class (MonadIO(liftIO))
import FokShell.Utils (bool')
import Data.List (partition, isSuffixOf, singleton, sort)
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory.OsPath (getCurrentDirectory, doesPathExist, getPermissions, Permissions (readable), listDirectory, pathIsSymbolicLink, doesDirectoryExist, setCurrentDirectory)
import System.Directory.Internal hiding (Directory)
import Control.Monad (forM_, forM)
import Data.Bool (bool)
import qualified Data.Text.Encoding as T
import Data.Bits (Bits(testBit))
import Prettyprinter
import FokShell.ShellNode
import FokShell.ShellNode.Table
import Control.Arrow (Arrow(second))
import Text.Regex.TDFA
import qualified System.OsPath as OP
import System.OsPath (encodeUtf)
import Data.Functor
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD (stderr)
import Control.Exception (try)
import Control.Monad.Error.Class (MonadError(throwError))
-- helpers {{{
round' :: Double -> Integer -> Double
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg
toUTC :: Real a => a -> UTCTime
toUTC t = (posixSecondsToUTCTime (realToFrac t) :: UTCTime)
calculateSize :: (Ord d, Fractional d, Num c) => d -> (d, c)
calculateSize x
    | x/1024 > 1 = second (+1) $ calculateSize (x/1024)
    | otherwise = (x, 0)

byteUnit :: (Eq a, Num a) => a -> T.Text
byteUnit 0 = "B"
byteUnit 1 = "KB"
byteUnit 2 = "MB"
byteUnit 3 = "GB"
byteUnit 4 = "TB"
byteUnit 5 = "PB"
byteUnit _ = "??"

-- }}}

-- Types {{{

data FileRole = Directory | Media | Executable | Device | Socket | Link | Archive | None
data FileR = FileR FilePath FileRole

instance PrettyAnn ShellAnn FileR where
  prettyAnn (FileR p Directory) = annotate AnnDirectory $ prettyAnn p
  prettyAnn (FileR p Media) = annotate AnnMedia $ prettyAnn p
  prettyAnn (FileR p Executable) = annotate AnnExecutable $ prettyAnn p
  prettyAnn (FileR p Device) = annotate AnnDevice $ prettyAnn p
  prettyAnn (FileR p Socket) = annotate AnnSocket $ prettyAnn p
  prettyAnn (FileR p Link) = annotate AnnLink $ prettyAnn p
  prettyAnn (FileR p Archive) = annotate AnnArchive $ prettyAnn p
  prettyAnn (FileR p None) = annotate AnnPlain $ prettyAnn p

newtype FileSize = FileSize FileOffset
instance PrettyAnn ShellAnn FileSize where
  prettyAnn (FileSize off) = annotate AnnInt . pretty $ T.show (round' size 2) <> " " <> byteUnit n
    where
      (size,n) = calculateSize $ fromIntegral off

newtype Perms = Perms FileMode

newtype Timestamp = Timestamp UTCTime

instance PrettyAnn ShellAnn Timestamp where
  prettyAnn (Timestamp t) = annotate AnnKeyword . pretty $ T.show t


userExec, userWrite, userRead,
  groupExec, groupWrite, groupRead,
  otherExec, otherWrite, otherRead
    :: FileMode -> Bool
userRead  = (`testBit` 8)
userWrite = (`testBit` 7)
userExec  = (`testBit` 6)
groupRead = (`testBit` 5)
groupWrite= (`testBit` 4)
groupExec = (`testBit` 3)
otherRead = (`testBit` 2)
otherWrite= (`testBit` 1)
otherExec = (`testBit` 0)

instance PrettyAnn ShellAnn Perms where
  prettyAnn (Perms mode) = hcat
    [ bool n r (userRead mode)
    , bool n w (userWrite mode)
    , bool n x (userExec mode)
    , bool n r (groupRead mode)
    , bool n w (groupWrite mode)
    , bool n x (groupExec mode)
    , bool n r (otherRead mode)
    , bool n w (otherWrite mode)
    , bool n x (otherExec mode)
    ]
    where
      n = annotate AnnNone $ pretty '-'
      r = annotate AnnRead $ pretty 'r'
      w = annotate AnnWrite $ pretty 'w'
      x = annotate AnnExecute $ pretty 'x'

-- }}}

-- ls {{{
ls_base :: (OsPath -> IO (Row a)) -> Process Void (Table a)
ls_base f args _ sink = do
  localRoot <- encodeUtf "."
  paths <- case args of
    [] -> pure . singleton $ localRoot
    _ -> mapM (encodeUtf . T.unpack) args
  exitCodes <- forM paths $ \path -> do
    liftIO $ doesPathExist path >>= bool
      (pure $ ExitFailure 2)
      (do
        perms <- getPermissions path
        bool' (readable perms)
          (pure $ ExitFailure 1)
          (do
          status <- getSymbolicLinkMetadata path
          let mkRow = (Table <$>) . mapM (f . bool id (path OP.</>) (path/=localRoot))
          found <- mkRow =<< bool' (isDirectory status)
            (pure [path])
            (sort <$> listDirectory path)
          sink.push found
          pure ExitSuccess
          )
      )
  case filter (/=ExitSuccess) exitCodes of
    (x:_) -> pure x
    [] -> pure ExitSuccess
type LsVerbose = '[ '("path", FileR), '("permission", Perms), '("owner", T.Text), '("group", T.Text), '("size", FileSize), '("access", Timestamp), '("modification", Timestamp), '("status_change", Timestamp) ]
lsl_row :: OsPath -> IO (Row LsVerbose)
lsl_row path = do
  stat <- getSymbolicLinkMetadata path
  let groupId = fileGroup stat
  let ownerId = fileOwner stat
  ownerEntry <- getUserEntryForID ownerId
  groupEntry <- getGroupEntryForID groupId
  path' <- OP.decodeUtf path
  let isDir = isDirectory stat
      -- todo: make it a REGEX!!!!
      isMedia = case reverse $ T.split (=='.') $ T.pack path' of
        (x:_) -> x =~ ("^*\\.(png|jpg|webp|webm|mp3|mkv|mp4|wav)$" :: T.Text)-- `elem` [".png", ".jpg", ".mp4"]
        _ -> False
      isExec = userExec x || groupExec x || otherExec x where x = fileMode stat 
      isDev = isBlockDevice stat
      isSock = isSocket stat
      isArchive = case reverse $ T.split (=='.') $ T.pack path' of
        (x:_) -> x =~ ("^*\\.(zip|gz|xz|tar)$" :: T.Text)-- `elem` [".png", ".jpg", ".mp4"]
        _ -> False
  isLink <- pathIsSymbolicLink path
  let role = case (isDir, isMedia, isExec, isDev, isSock, isLink, isArchive) of
            (True,_,_,_,_,_,_) -> Directory
            (_,True,_,_,_,_,_) -> Media
            (_,_,True,_,_,_,_) -> Executable
            (_,_,_,True,_,_,_) -> Device
            (_,_,_,_,True,_,_) -> Socket
            (_,_,_,_,_,True,_) -> Link
            (_,_,_,_,_,_,True) -> Archive
            _ -> None
  pure . Row $ (FileR path' role)
    >:: Perms (fileMode stat) >:: T.pack (userName ownerEntry) >:: T.pack (groupName groupEntry)
    >:: FileSize (fileSize stat) >:: Timestamp (toUTC $ accessTime stat) >:: Timestamp (toUTC $ modificationTime stat)
    >:: Timestamp (toUTC $ statusChangeTime stat) >:: CEmpty
lsl_ :: Process Void (Table LsVerbose)
lsl_ = ls_base lsl_row


type LsSmall = '[ '("path", FileR) ]
lss_row :: OsPath -> IO (Row LsSmall)
lss_row path = do
  stat <- getSymbolicLinkMetadata path
  path' <- OP.decodeUtf path
  let isDir = isDirectory stat
      isMedia = case reverse $ T.split (=='.') $ T.pack path' of
        (x:_) -> x =~ ("^*\\.(png|jpg|webp|webm|mp3|mkv|mp4|wav)$" :: T.Text)
        _ -> False
      isExec = userExec x || groupExec x || otherExec x where x = fileMode stat 
      isDev = isBlockDevice stat
      isSock = isSocket stat
      isArchive = case reverse $ T.split (=='.') $ T.pack path' of
        (x:_) -> x =~ ("^*\\.(zip|gz|xz|tar)$" :: T.Text)-- `elem` [".png", ".jpg", ".mp4"]
        _ -> False
  isLink <- pathIsSymbolicLink path
  let role = case (isDir, isMedia, isExec, isDev, isSock, isLink, isArchive) of
            (True,_,_,_,_,_,_) -> Directory
            (_,True,_,_,_,_,_) -> Media
            (_,_,True,_,_,_,_) -> Executable
            (_,_,_,True,_,_,_) -> Device
            (_,_,_,_,True,_,_) -> Socket
            (_,_,_,_,_,True,_) -> Link
            (_,_,_,_,_,_,True) -> Archive
            _ -> None
  pure . Row $ FileR path' role >:: CEmpty
lss_ :: Process Void (Table LsSmall)
lss_ = ls_base lss_row


ls_ :: Process Void SomeTable
ls_ args' _source sink = do
  let (flags, args) = partition (T.isPrefixOf "-") args'
  bool' ("-l" `elem` flags)
    (lss_ args _source sink{push = sink.push . SomeTable, finish = sink.finish})
    (lsl_ args _source sink{push = sink.push . SomeTable, finish = sink.finish})
-- }}}

-- cd {{{
cd_ :: Process Void Void
cd_ args _ _ = case args of
  [] -> pure ExitSuccess
  [x] -> do
    path <- encodeUtf (T.unpack x)
    liftIO (try $ setCurrentDirectory path) >>= \case
      Left e -> throwError (ErrorIO e)
      Right _-> pure ExitSuccess
    --liftIO $ doesDirectoryExist path >>= bool
    --  (T.hPutStrLn stderr ("cd: " <> x <> ": no such file or directory") >> pure (ExitFailure 1))
    --  (setCurrentDirectory path $> ExitSuccess)
  _ -> liftIO $ T.hPutStrLn stderr "cd: too many arguments" >> pure (ExitFailure 2)
-- }}}
-- vim: foldmethod=marker
