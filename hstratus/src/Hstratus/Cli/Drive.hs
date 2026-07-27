{-# LANGUAGE TypeApplications #-}

{- |
Module      : Hstratus.Cli.Drive
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

CLI subcommands for iCloud Drive (list, copy, download).
-}
module Hstratus.Cli.Drive
  ( DriveCommand (..)
  , LsFormat (..)
  , LsOpts (..)
  , CpOpts (..)
  , CpDest (..)
  , driveParser
  , runDrive
  , resolveLocalDest
  , formatSize
  , displayNode
  )
where

import Control.Exception (catch)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HStratus.Drive
  ( DriveApi
  , DriveError
  , DriveNode (..)
  , DriveNodeId
  , FileData (..)
  , FolderData (..)
  , downloadFile
  , driveRoot
  , fileName
  , listFolder
  , mkDriveApi
  , selectFileNode
  )
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, onServiceError, runWithApi)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (die)
import System.FilePath (joinPath, takeDirectory, (</>))


-- | Top-level Drive subcommand.
data DriveCommand
  = -- | list the contents of a Drive folder
    DriveLs !LsOpts
  | -- | download a file from Drive
    DriveCp !CpOpts
  deriving (Eq, Show)


-- | Controls size display in @ls@ output.
data LsFormat
  = -- | raw bytes (default)
    LsBytes
  | -- | powers of 1024: KiB, MiB, GiB, …
    LsHuman
  | -- | powers of 1000: KB, MB, GB, …
    LsSI
  deriving (Eq, Ord, Show)


-- | Options for the @drive ls@ subcommand.
data LsOpts = LsOpts
  { lsPath :: ![Text]
  -- ^ slash-separated path segments from the Drive root; empty means root
  , lsFormat :: !LsFormat
  -- ^ size display format; controlled by @--human@ and @--si@
  , lsCommon :: !CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Destination specifier for the @drive cp@ subcommand.
data CpDest
  = -- | mirror the Drive path under the given local root directory
    CpDestRoot !FilePath
  | -- | write the file to the exact local path
    CpDestOutput !FilePath
  deriving (Eq, Show)


-- | Options for the @drive cp@ subcommand.
data CpOpts = CpOpts
  { cpSrcPath :: !(NonEmpty Text)
  -- ^ slash-separated path segments identifying the source file in Drive
  , cpDest :: !(Maybe CpDest)
  -- ^ local destination; @Nothing@ defaults to @~/icloud-drive/\<path\>@
  , cpVerbose :: !Bool
  -- ^ when @True@, print the downloaded file entry in @ls@ style after download
  , cpFormat :: !LsFormat
  -- ^ size format for verbose output; controlled by @--human@ and @--si@
  , cpCommon :: !CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Optparse-applicative parser for the @drive@ subcommand.
driveParser :: Parser DriveCommand
driveParser =
  subparser
    ( command
        "ls"
        ( info
            (DriveLs <$> lsOptsParser <**> helper)
            (progDesc "List contents of a Drive folder (default: root)")
        )
        <> command
          "cp"
          ( info
              (DriveCp <$> cpOptsParser <**> helper)
              (progDesc "Download a file from Drive to the local filesystem")
          )
    )


cpOptsParser :: Parser CpOpts
cpOptsParser =
  CpOpts
    <$> argument
      ( eitherReader $ \s ->
          let segs = filter (not . Text.null) (Text.splitOn (Text.pack "/") (Text.pack s))
           in case NE.nonEmpty segs of
                Nothing -> Left "PATH must not be empty"
                Just ne -> Right ne
      )
      (metavar "PATH" <> help "Slash-separated path to the file in Drive")
    <*> optional
      ( (CpDestRoot <$> strOption (long "root" <> metavar "DIR" <> help "Copy under DIR, mirroring the Drive path"))
          <|> (CpDestOutput <$> strOption (long "output" <> metavar "FILE" <> help "Copy to the exact local path FILE"))
      )
    <*> switch (long "verbose" <> help "Print downloaded file entry in ls style")
    <*> lsFormatParser
    <*> commonOptsParser


lsOptsParser :: Parser LsOpts
lsOptsParser =
  LsOpts
    <$> fmap
      (maybe [] (filter (not . Text.null) . Text.splitOn (Text.pack "/") . Text.pack))
      (optional (argument str (metavar "[PATH]" <> help "Slash-separated path from root (e.g. Documents/Work)")))
    <*> lsFormatParser
    <*> commonOptsParser


lsFormatParser :: Parser LsFormat
lsFormatParser =
  flag' LsHuman (long "human" <> help "Human-readable sizes (KiB, MiB, …)")
    <|> flag' LsSI (long "si" <> help "SI sizes (KB, MB, …)")
    <|> pure LsBytes


-- | Dispatch a 'DriveCommand' to its handler.
runDrive :: DriveCommand -> IO ()
runDrive (DriveLs opts) = runLs opts
runDrive (DriveCp opts) = runCp opts


runCp :: CpOpts -> IO ()
runCp opts =
  withDriveApi (cpCommon opts) $ \da -> do
    root <- driveRoot da
    fd <- navigateToFile da (fnId root) (cpSrcPath opts)
    dest <- resolveLocalDest opts (cpSrcPath opts)
    createDirectoryIfMissing True (takeDirectory dest)
    bytes <- downloadFile da fd
    LBS.writeFile dest bytes
    when (cpVerbose opts) $ do
      let verboseOpts = LsOpts{lsPath = [], lsFormat = cpFormat opts, lsCommon = cpCommon opts}
      putStrLn (displayNode verboseOpts (DriveFile fd))
    putStrLn $ "Downloaded to " <> dest


navigateToFile :: DriveApi -> DriveNodeId -> NonEmpty Text -> IO FileData
navigateToFile da nid (name :| []) = do
  children <- listFolder da nid
  case selectFileNode name children of
    Just (DriveFile fd) -> pure fd
    Just (DriveFolder _) -> die $ "Not a file: " <> Text.unpack name
    Nothing -> die $ "File not found: " <> Text.unpack name
navigateToFile da nid (seg :| (s : rest)) = do
  children <- listFolder da nid
  case selectFileNode seg children of
    Nothing -> die $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> die $ "Not a folder: " <> Text.unpack seg
    Just (DriveFolder fd) -> navigateToFile da (fnId fd) (s :| rest)


-- | Resolve the local destination path for a download, expanding @~@ via 'getHomeDirectory' when needed.
resolveLocalDest :: CpOpts -> NonEmpty Text -> IO FilePath
resolveLocalDest (CpOpts{cpDest = Just (CpDestOutput out)}) _ = pure out
resolveLocalDest (CpOpts{cpDest = Just (CpDestRoot topDir)}) segs =
  pure $ topDir </> joinPath (map Text.unpack (NE.toList segs))
resolveLocalDest _ segs = do
  home <- getHomeDirectory
  pure $ home </> "icloud-drive" </> joinPath (map Text.unpack (NE.toList segs))


runLs :: LsOpts -> IO ()
runLs opts =
  withDriveApi (lsCommon opts) $ \da -> do
    root <- driveRoot da
    nid <- navigatePath da (fnId root) (lsPath opts)
    nodes <- listFolder da nid
    mapM_ (putStrLn . displayNode opts) nodes


navigatePath :: DriveApi -> DriveNodeId -> [Text] -> IO DriveNodeId
navigatePath _ nid [] = pure nid
navigatePath da nid (seg : segs) = do
  children <- listFolder da nid
  case selectFileNode seg children of
    Nothing -> die $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> die $ "Not a folder: " <> Text.unpack seg
    Just (DriveFolder fd) -> navigatePath da (fnId fd) segs


-- | Format a file size for display according to the given 'LsFormat'.
formatSize :: LsFormat -> Int64 -> String
formatSize LsBytes n = show n <> " bytes"
formatSize LsHuman n
  | n < 1024 = show n <> " bytes"
  | n < 1024 * 1024 = showFrac (fromIntegral n / 1024) <> " KiB"
  | n < 1024 * 1024 * 1024 = showFrac (fromIntegral n / (1024 * 1024)) <> " MiB"
  | otherwise = showFrac (fromIntegral n / (1024 * 1024 * 1024)) <> " GiB"
formatSize LsSI n
  | n < 1000 = show n <> " bytes"
  | n < 1000 * 1000 = showFrac (fromIntegral n / 1000) <> " KB"
  | n < 1000 * 1000 * 1000 = showFrac (fromIntegral n / (1000 * 1000)) <> " MB"
  | otherwise = showFrac (fromIntegral n / (1000 * 1000 * 1000)) <> " GB"


showFrac :: Double -> String
showFrac x =
  let tenths = round (x * 10) :: Int
      whole = tenths `div` 10
      frac = tenths `mod` 10
   in show whole <> "." <> show frac


{- | Format a single 'DriveNode' for display in @ls@ output.

Folders are prefixed with @d@; files with a space.  The size column uses
the 'LsFormat' from the supplied 'LsOpts'.
-}
displayNode :: LsOpts -> DriveNode -> String
displayNode _ (DriveFolder fd) = "d " <> Text.unpack (fnName fd)
displayNode opts (DriveFile fd) =
  "  " <> Text.unpack (fileName fd) <> sizeStr
 where
  sizeStr = case fdSize fd of
    Nothing -> ""
    Just n -> "  (" <> formatSize (lsFormat opts) n <> ")"


withDriveApi :: CommonOpts -> (DriveApi -> IO ()) -> IO ()
withDriveApi opts runAction =
  runWithApi opts (\ad sess api -> mkDriveApi ad sess api >>= runAction)
    `catch` onServiceError @DriveError
