{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hstratus.Cli.DriveSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Tests for the iCloud Drive CLI subcommands.
-}
module Hstratus.Cli.DriveSpec (spec) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Drive
  ( CpDest (..)
  , CpOpts (..)
  , DriveCommand (..)
  , LsFormat (..)
  , LsOpts (..)
  , displayNode
  , formatSize
  , resolveLocalDest
  )
import Network.HStratus.Drive (DriveNode (..), FileData (..), FolderData (..))
import Network.HStratus.Http.Cli (CommonOpts (..))
import Options.Applicative
  ( ParserResult (..)
  , defaultPrefs
  , execParserPure
  , renderFailure
  )
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Benri (endsRight)


parseCmd :: [String] -> IO (Either String TopCommand)
parseCmd args =
  pure $ case execParserPure defaultPrefs cliParser args of
    Success cmd -> Right cmd
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"


defaultOpts :: CommonOpts
defaultOpts = CommonOpts False False Nothing False False


defaultLsOpts :: LsOpts
defaultLsOpts = LsOpts [] LsBytes defaultOpts


defaultCpOpts :: CpOpts
defaultCpOpts = CpOpts ("report.pdf" :| []) Nothing False LsBytes defaultOpts


testFolderNode :: DriveNode
testFolderNode =
  DriveFolder
    FolderData
      { fnId = "folder-id"
      , fnEtag = "etag"
      , fnName = "Desktop"
      , fnZone = "com.apple.CloudDocs"
      , fnDateCreated = Nothing
      }


testFileNode :: DriveNode
testFileNode =
  DriveFile
    FileData
      { fdId = "file-id"
      , fdDocId = "doc-id"
      , fdEtag = "etag"
      , fdName = "notes"
      , fdExtension = Just "txt"
      , fdZone = "com.apple.CloudDocs"
      , fdSize = Just (1024 * 1024)
      , fdDateCreated = Nothing
      , fdDateModified = Nothing
      }


spec :: Spec
spec = do
  describe "formatSize" $ do
    it "LsBytes 0 returns raw bytes" $
      formatSize LsBytes 0 `shouldBe` "0 bytes"
    it "LsBytes 1023 returns raw bytes" $
      formatSize LsBytes 1023 `shouldBe` "1023 bytes"
    it "LsHuman 1023 returns bytes (below 1024 threshold)" $
      formatSize LsHuman 1023 `shouldBe` "1023 bytes"
    it "LsHuman 1024 returns 1.0 KiB" $
      formatSize LsHuman 1024 `shouldBe` "1.0 KiB"
    it "LsHuman 1536 returns 1.5 KiB" $
      formatSize LsHuman 1536 `shouldBe` "1.5 KiB"
    it "LsHuman 1048576 returns 1.0 MiB" $
      formatSize LsHuman (1024 * 1024) `shouldBe` "1.0 MiB"
    it "LsSI 999 returns bytes (below 1000 threshold)" $
      formatSize LsSI 999 `shouldBe` "999 bytes"
    it "LsSI 1000 returns 1.0 KB" $
      formatSize LsSI 1000 `shouldBe` "1.0 KB"
    it "LsSI 1000000 returns 1.0 MB" $
      formatSize LsSI (1000 * 1000) `shouldBe` "1.0 MB"

  describe "displayNode type character" $ do
    it "folder node line starts with 'd'" $
      case displayNode defaultLsOpts testFolderNode of
        (c : _) -> c `shouldBe` 'd'
        [] -> expectationFailure "expected non-empty string"
    it "file node line starts with ' '" $
      case displayNode defaultLsOpts testFileNode of
        (c : _) -> c `shouldBe` ' '
        [] -> expectationFailure "expected non-empty string"

  describe "resolveLocalDest" $ do
    it "returns the exact output path when --output is set" $
      resolveLocalDest defaultCpOpts{cpDest = Just (CpDestOutput "/tmp/out.pdf")} ("report.pdf" :| [])
        `shouldReturn` "/tmp/out.pdf"

    it "mirrors the Drive path under --root DIR" $
      resolveLocalDest defaultCpOpts{cpDest = Just (CpDestRoot "/tmp/dl")} ("Documents" :| ["report.pdf"])
        `shouldReturn` "/tmp/dl/Documents/report.pdf"

    it "uses ~/icloud-drive as the default destination" $ do
      home <- getHomeDirectory
      resolveLocalDest defaultCpOpts ("Documents" :| ["report.pdf"])
        `shouldReturn` home </> "icloud-drive" </> "Documents/report.pdf"

  describe "drive parser" $ do
    it "parses drive ls with no argument (root)" $
      parseCmd ["drive", "ls"]
        `endsRight` DriveCmd (DriveLs (LsOpts [] LsBytes defaultOpts))

    it "parses drive ls PATH" $
      parseCmd ["drive", "ls", "Documents/Work"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents", "Work"] LsBytes defaultOpts))

    it "parses drive ls PATH with common flags" $
      parseCmd ["drive", "ls", "Documents", "--china", "--log"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents"] LsBytes defaultOpts{optChina = True, optLog = True}))

    it "treats a leading slash in PATH as root listing" $
      parseCmd ["drive", "ls", "/Documents/Work"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents", "Work"] LsBytes defaultOpts))

    it "drive ls has lsFormat = LsBytes by default" $
      parseCmd ["drive", "ls"]
        `endsRight` DriveCmd (DriveLs (LsOpts [] LsBytes defaultOpts))

    it "drive ls --human sets lsFormat = LsHuman" $
      parseCmd ["drive", "ls", "--human"]
        `endsRight` DriveCmd (DriveLs (LsOpts [] LsHuman defaultOpts))

    it "drive ls --si sets lsFormat = LsSI" $
      parseCmd ["drive", "ls", "--si"]
        `endsRight` DriveCmd (DriveLs (LsOpts [] LsSI defaultOpts))

    it "drive ls --human --si fails (mutual exclusion)" $ do
      result <- parseCmd ["drive", "ls", "--human", "--si"]
      result `shouldSatisfy` isLeft

    it "parses drive cp PATH with no dest option" $
      parseCmd ["drive", "cp", "Documents/report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) Nothing False LsBytes defaultOpts))

    it "parses drive cp PATH --root DIR" $
      parseCmd ["drive", "cp", "Documents/Work/report.pdf", "--root", "/tmp/dl"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["Work", "report.pdf"]) (Just (CpDestRoot "/tmp/dl")) False LsBytes defaultOpts))

    it "parses drive cp PATH --output FILE" $
      parseCmd ["drive", "cp", "Documents/report.pdf", "--output", "/tmp/report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) (Just (CpDestOutput "/tmp/report.pdf")) False LsBytes defaultOpts))

    it "parses drive cp single-segment PATH" $
      parseCmd ["drive", "cp", "report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("report.pdf" :| []) Nothing False LsBytes defaultOpts))

    it "drive cp PATH has cpVerbose = False, cpFormat = LsBytes by default" $
      parseCmd ["drive", "cp", "report.pdf"]
        `endsRight` DriveCmd (DriveCp (CpOpts ("report.pdf" :| []) Nothing False LsBytes defaultOpts))

    it "drive cp PATH --verbose sets cpVerbose = True" $
      parseCmd ["drive", "cp", "report.pdf", "--verbose"]
        `endsRight` DriveCmd (DriveCp (CpOpts ("report.pdf" :| []) Nothing True LsBytes defaultOpts))

    it "drive cp PATH --verbose --human sets cpFormat = LsHuman" $
      parseCmd ["drive", "cp", "report.pdf", "--verbose", "--human"]
        `endsRight` DriveCmd (DriveCp (CpOpts ("report.pdf" :| []) Nothing True LsHuman defaultOpts))

    it "rejects --root and --output together at parse time" $ do
      result <- parseCmd ["drive", "cp", "Documents/report.pdf", "--root", "/tmp/dl", "--output", "/tmp/out.pdf"]
      result `shouldSatisfy` isLeft
