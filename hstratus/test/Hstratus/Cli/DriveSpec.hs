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
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Drive
  ( CpDest (..)
  , CpOpts (..)
  , DriveCommand (..)
  , LsFilter (..)
  , LsFormat (..)
  , LsOpts (..)
  , LsSort (..)
  , displayNode
  , filterNodes
  , formatSize
  , nodeDate
  , nodeName
  , resolveLocalDest
  , sortNodes
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
defaultLsOpts =
  LsOpts
    { lsPath = []
    , lsFormat = LsBytes
    , lsSort = LsSortDefault
    , lsReverse = False
    , lsLong = False
    , lsIds = False
    , lsFilter = LsFilterAll
    , lsCommon = defaultOpts
    }


defaultCpOpts :: CpOpts
defaultCpOpts = CpOpts ("report.pdf" :| []) Nothing False LsBytes defaultOpts


-- | Two fixed UTC timestamps for sort and date tests.  t2 is newer than t1.
t1 :: UTCTime
t1 = UTCTime (fromGregorian 2026 7 1) (secondsToDiffTime (10 * 3600))


t2 :: UTCTime
t2 = UTCTime (fromGregorian 2026 7 27) (secondsToDiffTime (9 * 3600))


mkFolder :: Text -> Maybe UTCTime -> DriveNode
mkFolder n d =
  DriveFolder
    FolderData
      { fnId = "folder-id"
      , fnEtag = "etag"
      , fnName = n
      , fnZone = "com.apple.CloudDocs"
      , fnDateCreated = d
      }


mkFile :: Text -> Maybe UTCTime -> Maybe UTCTime -> DriveNode
mkFile n created modified =
  DriveFile
    FileData
      { fdId = "file-id"
      , fdDocId = "doc-id"
      , fdEtag = "etag"
      , fdName = n
      , fdExtension = Nothing
      , fdZone = "com.apple.CloudDocs"
      , fdSize = Nothing
      , fdDateCreated = created
      , fdDateModified = modified
      }


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

  describe "sortNodes" $ do
    it "LsSortName False sorts alphabetically by display name" $ do
      let nodes = [mkFolder "Zebra" Nothing, mkFile "apple" Nothing Nothing, mkFolder "Banana" Nothing]
      map nodeName (sortNodes LsSortName False nodes) `shouldBe` ["Banana", "Zebra", "apple"]

    it "LsSortName True sorts in reversed alphabetical order" $ do
      let nodes = [mkFolder "Zebra" Nothing, mkFile "apple" Nothing Nothing, mkFolder "Banana" Nothing]
      map nodeName (sortNodes LsSortName True nodes) `shouldBe` ["apple", "Zebra", "Banana"]

    it "LsSortDate False puts newest first when files have fdDateModified" $ do
      let nodes = [mkFile "old" (Just t1) (Just t1), mkFile "new" (Just t2) (Just t2)]
      map nodeName (sortNodes LsSortDate False nodes) `shouldBe` ["new", "old"]

    it "LsSortDate True puts oldest first" $ do
      let nodes = [mkFile "old" (Just t1) (Just t1), mkFile "new" (Just t2) (Just t2)]
      map nodeName (sortNodes LsSortDate True nodes) `shouldBe` ["old", "new"]

    it "LsSortDate False uses fdDateModified for files and fnDateCreated for folders" $ do
      let folder = mkFolder "Folder" (Just t1)
          file = mkFile "file" (Just t1) (Just t2)
      map nodeName (sortNodes LsSortDate False [folder, file]) `shouldBe` ["file", "Folder"]

    it "LsSortDate False falls back to fdDateCreated when fdDateModified is Nothing" $ do
      let fileCreatedLate = mkFile "file" (Just t2) Nothing
          fileModifiedEarly = mkFile "earlier" (Just t1) (Just t1)
      map nodeName (sortNodes LsSortDate False [fileModifiedEarly, fileCreatedLate])
        `shouldBe` ["file", "earlier"]

    it "LsSortDate False puts nodes with no date last" $ do
      let fileNoDate = mkFile "no-date" Nothing Nothing
          fileWithDate = mkFile "has-date" (Just t1) (Just t1)
      map nodeName (sortNodes LsSortDate False [fileNoDate, fileWithDate])
        `shouldBe` ["has-date", "no-date"]

    it "LsSortDefault False preserves input order" $ do
      let nodes = [mkFolder "B" Nothing, mkFile "a" Nothing Nothing, mkFolder "C" Nothing]
      sortNodes LsSortDefault False nodes `shouldBe` nodes

  describe "filterNodes" $ do
    it "LsFilterFolders returns only folders" $ do
      let mixed = [mkFolder "Folder" Nothing, mkFile "file" Nothing Nothing]
      filterNodes LsFilterFolders mixed `shouldBe` [mkFolder "Folder" Nothing]

    it "LsFilterFiles returns only files" $ do
      let mixed = [mkFolder "Folder" Nothing, mkFile "file" Nothing Nothing]
      filterNodes LsFilterFiles mixed `shouldBe` [mkFile "file" Nothing Nothing]

    it "LsFilterAll returns the list unchanged" $ do
      let mixed = [mkFolder "Folder" Nothing, mkFile "file" Nothing Nothing]
      filterNodes LsFilterAll mixed `shouldBe` mixed

  describe "nodeDate" $ do
    it "folder returns fnDateCreated" $
      nodeDate (mkFolder "F" (Just t1)) `shouldBe` Just t1

    it "file with fdDateModified returns fdDateModified" $
      nodeDate (mkFile "f" (Just t1) (Just t2)) `shouldBe` Just t2

    it "file with fdDateModified = Nothing returns fdDateCreated" $
      nodeDate (mkFile "f" (Just t1) Nothing) `shouldBe` Just t1

    it "file with both dates Nothing returns Nothing" $
      nodeDate (mkFile "f" Nothing Nothing) `shouldBe` Nothing

  describe "drive parser" $ do
    it "parses drive ls with no argument (root)" $
      parseCmd ["drive", "ls"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts)

    it "parses drive ls PATH" $
      parseCmd ["drive", "ls", "Documents/Work"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsPath = ["Documents", "Work"]})

    it "parses drive ls PATH with common flags" $
      parseCmd ["drive", "ls", "Documents", "--china", "--log"]
        `endsRight` DriveCmd
          (DriveLs defaultLsOpts{lsPath = ["Documents"], lsCommon = defaultOpts{optChina = True, optLog = True}})

    it "treats a leading slash in PATH as root listing" $
      parseCmd ["drive", "ls", "/Documents/Work"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsPath = ["Documents", "Work"]})

    it "drive ls has lsFormat = LsBytes by default" $
      parseCmd ["drive", "ls"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts)

    it "drive ls --human sets lsFormat = LsHuman" $
      parseCmd ["drive", "ls", "--human"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsFormat = LsHuman})

    it "drive ls --si sets lsFormat = LsSI" $
      parseCmd ["drive", "ls", "--si"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsFormat = LsSI})

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

    it "drive ls --sort=name sets lsSort = LsSortName" $
      parseCmd ["drive", "ls", "--sort=name"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsSort = LsSortName})

    it "drive ls --sort=date sets lsSort = LsSortDate" $
      parseCmd ["drive", "ls", "--sort=date"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsSort = LsSortDate})

    it "drive ls --sort=unknown fails" $ do
      result <- parseCmd ["drive", "ls", "--sort=unknown"]
      result `shouldSatisfy` isLeft

    it "drive ls --reverse sets lsReverse = True" $
      parseCmd ["drive", "ls", "--reverse"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsReverse = True})

    it "drive ls --long sets lsLong = True" $
      parseCmd ["drive", "ls", "--long"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsLong = True})

    it "drive ls --ids sets lsIds = True" $
      parseCmd ["drive", "ls", "--ids"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsIds = True})

    it "drive ls --folders-only sets lsFilter = LsFilterFolders" $
      parseCmd ["drive", "ls", "--folders-only"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsFilter = LsFilterFolders})

    it "drive ls --files-only sets lsFilter = LsFilterFiles" $
      parseCmd ["drive", "ls", "--files-only"]
        `endsRight` DriveCmd (DriveLs defaultLsOpts{lsFilter = LsFilterFiles})

    it "drive ls --folders-only --files-only fails (mutual exclusion)" $ do
      result <- parseCmd ["drive", "ls", "--folders-only", "--files-only"]
      result `shouldSatisfy` isLeft
