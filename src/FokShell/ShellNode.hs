module FokShell.ShellNode where
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

data ShellAnn = AnnValid | AnnInvalid | AnnKeyword
              | AnnNone | AnnRead | AnnWrite | AnnExecute
              | AnnColumn
              | AnnPlain | AnnDirectory | AnnMedia | AnnExecutable | AnnDevice | AnnSocket | AnnLink | AnnArchive
              | AnnInt

instance PrettyAnn ShellAnn BS.ByteString where
  prettyAnn = pretty . T.decodeUtf8

shellAnn :: ShellAnn -> AnsiStyle
shellAnn AnnValid = color Green
shellAnn AnnInvalid = color Red
shellAnn AnnKeyword = color Magenta
shellAnn AnnNone = colorDull Black
shellAnn AnnRead = colorDull Green
shellAnn AnnWrite = colorDull Magenta
shellAnn AnnExecute = colorDull Red
shellAnn AnnColumn = color Green <> bold
shellAnn AnnPlain = color White
shellAnn AnnDirectory = color Blue <> bold
shellAnn AnnMedia = color Magenta <> bold
shellAnn AnnExecutable = color Green <> bold
shellAnn AnnLink = colorDull Blue <> bold
shellAnn AnnDevice = bgColor Yellow <> color Black <> bold
shellAnn AnnArchive = color Red <> bold
shellAnn AnnSocket = bgColor Magenta <> bold
shellAnn AnnInt = colorDull Blue

data ShellNode where ShellNode :: PrettyAnn ShellAnn a => a -> ShellNode

instance PrettyAnn ShellAnn ShellNode where
  prettyAnn (ShellNode a) = prettyAnn a 
