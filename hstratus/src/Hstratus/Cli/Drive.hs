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
  , LsOpts (..)
  , CpOpts (..)
  , CpDest (..)
  , driveParser
  , runDrive
  , resolveLocalDest
  )
where

import Control.Exception (catch)
import qualified Data.ByteString.Lazy as LBS
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


-- | Options for the @drive ls@ subcommand.
data LsOpts = LsOpts
  { lsPath :: ![Text]
  -- ^ slash-separated path segments from the Drive root; empty means root
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
    <*> commonOptsParser


lsOptsParser :: Parser LsOpts
lsOptsParser =
  LsOpts
    <$> fmap
      (maybe [] (filter (not . Text.null) . Text.splitOn (Text.pack "/") . Text.pack))
      (optional (argument str (metavar "[PATH]" <> help "Slash-separated path from root (e.g. Documents/Work)")))
    <*> commonOptsParser


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
resolveLocalDest (CpOpts{cpDest = Just (CpDestRoot root)}) segs =
  pure $ root </> joinPath (map Text.unpack (NE.toList segs))
resolveLocalDest _ segs = do
  home <- getHomeDirectory
  pure $ home </> "icloud-drive" </> joinPath (map Text.unpack (NE.toList segs))


runLs :: LsOpts -> IO ()
runLs opts =
  withDriveApi (lsCommon opts) $ \da -> do
    root <- driveRoot da
    nid <- navigatePath da (fnId root) (lsPath opts)
    nodes <- listFolder da nid
    mapM_ printNode nodes


navigatePath :: DriveApi -> DriveNodeId -> [Text] -> IO DriveNodeId
navigatePath _ nid [] = pure nid
navigatePath da nid (seg : segs) = do
  children <- listFolder da nid
  case selectFileNode seg children of
    Nothing -> die $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> die $ "Not a folder: " <> Text.unpack seg
    Just (DriveFolder fd) -> navigatePath da (fnId fd) segs


printNode :: DriveNode -> IO ()
printNode (DriveFolder fd) =
  putStrLn $ "FOLDER  " <> Text.unpack (fnName fd)
printNode (DriveFile fd) =
  putStrLn $ "FILE    " <> Text.unpack (fileName fd) <> sizeStr
 where
  sizeStr = case fdSize fd of
    Nothing -> ""
    Just n -> "  (" <> show n <> " bytes)"


withDriveApi :: CommonOpts -> (DriveApi -> IO ()) -> IO ()
withDriveApi opts runAction =
  runWithApi opts (\ad sess api -> mkDriveApi ad sess api >>= runAction)
    `catch` onServiceError @DriveError
