{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.DriveSpec (spec) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Drive (CpDest (..), CpOpts (..), DriveCommand (..), LsOpts (..), resolveLocalDest)
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


defaultCpOpts :: CpOpts
defaultCpOpts = CpOpts ("report.pdf" :| []) Nothing defaultOpts


spec :: Spec
spec = do
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
        `endsRight` DriveCmd (DriveLs (LsOpts [] defaultOpts))

    it "parses drive ls PATH" $
      parseCmd ["drive", "ls", "Documents/Work"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents", "Work"] defaultOpts))

    it "parses drive ls PATH with common flags" $
      parseCmd ["drive", "ls", "Documents", "--china", "--log"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents"] defaultOpts{optChina = True, optLog = True}))

    it "treats a leading slash in PATH as root listing" $
      parseCmd ["drive", "ls", "/Documents/Work"]
        `endsRight` DriveCmd (DriveLs (LsOpts ["Documents", "Work"] defaultOpts))

    it "parses drive cp PATH with no dest option" $
      parseCmd ["drive", "cp", "Documents/report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) Nothing defaultOpts))

    it "parses drive cp PATH --root DIR" $
      parseCmd ["drive", "cp", "Documents/Work/report.pdf", "--root", "/tmp/dl"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["Work", "report.pdf"]) (Just (CpDestRoot "/tmp/dl")) defaultOpts))

    it "parses drive cp PATH --output FILE" $
      parseCmd ["drive", "cp", "Documents/report.pdf", "--output", "/tmp/report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) (Just (CpDestOutput "/tmp/report.pdf")) defaultOpts))

    it "parses drive cp single-segment PATH" $
      parseCmd ["drive", "cp", "report.pdf"]
        `endsRight` DriveCmd
          (DriveCp (CpOpts ("report.pdf" :| []) Nothing defaultOpts))

    it "rejects --root and --output together at parse time" $ do
      result <- parseCmd ["drive", "cp", "Documents/report.pdf", "--root", "/tmp/dl", "--output", "/tmp/out.pdf"]
      result `shouldSatisfy` isLeft
