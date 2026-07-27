{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hstratus.Cli.NotesSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Tests for the Notes CLI subcommand parser, folder-name helpers, and filename
allocation utilities in 'Hstratus.Cli.Notes'.
-}
module Hstratus.Cli.NotesSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Notes
  ( ExportFolderDest (..)
  , ExportFolderOpts (..)
  , GetFormat (..)
  , GetOpts (..)
  , ListNotesOpts (..)
  , NotesCommand (..)
  , findFolderByName
  , noteBasename
  , resolveExportDest
  , uniqueBasenames
  )
import Network.HStratus.Http.Cli (CommonOpts (..))
import Network.HStratus.Notes.Note (FolderId (..), NoteFolder (..), NoteId (..))
import Options.Applicative
  ( ParserResult (..)
  , defaultPrefs
  , execParserPure
  , renderFailure
  )
import Test.Hspec
import Test.Hspec.Benri (endsLeft_, endsRight)


parseCmd :: [String] -> IO (Either String TopCommand)
parseCmd args =
  pure $ case execParserPure defaultPrefs cliParser args of
    Success cmd -> Right cmd
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"


defaultOpts :: CommonOpts
defaultOpts = CommonOpts False False Nothing False False


spec :: Spec
spec = do
  describe "notes parser" $ do
    it "parses notes list-note-folders" $
      parseCmd ["notes", "list-note-folders"]
        `endsRight` NotesCmd (NotesListFolders defaultOpts)

    it "parses notes list-notes --folder NAME" $
      parseCmd ["notes", "list-notes", "--folder", "TukTuk"]
        `endsRight` NotesCmd (NotesListNotes (ListNotesOpts (Just "TukTuk") defaultOpts))

    it "parses notes get NOTE_ID (default format is markdown)" $
      parseCmd ["notes", "get", "Note/ABCD-1234"]
        `endsRight` NotesCmd (NotesGet (GetOpts (NoteId "Note/ABCD-1234") GetMarkdown defaultOpts))

    it "parses notes get NOTE_ID --format markdown" $
      parseCmd ["notes", "get", "Note/ABCD-1234", "--format", "markdown"]
        `endsRight` NotesCmd (NotesGet (GetOpts (NoteId "Note/ABCD-1234") GetMarkdown defaultOpts))

    it "parses notes get NOTE_ID --format text" $
      parseCmd ["notes", "get", "Note/ABCD-1234", "--format", "text"]
        `endsRight` NotesCmd (NotesGet (GetOpts (NoteId "Note/ABCD-1234") GetText defaultOpts))

    it "parses notes get NOTE_ID with --china" $
      parseCmd ["notes", "get", "Note/ABCD-1234", "--china"]
        `endsRight` NotesCmd (NotesGet (GetOpts (NoteId "Note/ABCD-1234") GetMarkdown defaultOpts{optChina = True}))

    it "rejects notes get with no argument" $ do
      endsLeft_ $ parseCmd ["notes", "get"]

    it "rejects notes get with unknown --format value" $ do
      endsLeft_ $ parseCmd ["notes", "get", "Note/ABCD-1234", "--format", "html"]

  describe "notes export-folder parser" $ do
    it "parses export-folder FOLDER with defaults" $
      parseCmd ["notes", "export-folder", "TukTuk"]
        `endsRight` NotesCmd
          (NotesExportFolder (ExportFolderOpts "TukTuk" Nothing GetMarkdown defaultOpts))

    it "parses export-folder FOLDER --root DIR" $
      parseCmd ["notes", "export-folder", "TukTuk", "--root", "/data"]
        `endsRight` NotesCmd
          (NotesExportFolder (ExportFolderOpts "TukTuk" (Just (ExportFolderRoot "/data")) GetMarkdown defaultOpts))

    it "parses export-folder FOLDER --output DIR" $
      parseCmd ["notes", "export-folder", "TukTuk", "--output", "/tmp/out"]
        `endsRight` NotesCmd
          (NotesExportFolder (ExportFolderOpts "TukTuk" (Just (ExportFolderOutput "/tmp/out")) GetMarkdown defaultOpts))

    it "parses export-folder FOLDER --format text" $
      parseCmd ["notes", "export-folder", "TukTuk", "--format", "text"]
        `endsRight` NotesCmd
          (NotesExportFolder (ExportFolderOpts "TukTuk" Nothing GetText defaultOpts))

    it "parses export-folder FOLDER --format markdown" $
      parseCmd ["notes", "export-folder", "TukTuk", "--format", "markdown"]
        `endsRight` NotesCmd
          (NotesExportFolder (ExportFolderOpts "TukTuk" Nothing GetMarkdown defaultOpts))

    it "rejects export-folder with unknown --format value" $
      endsLeft_ $
        parseCmd ["notes", "export-folder", "TukTuk", "--format", "html"]

    it "rejects export-folder with no FOLDER argument" $
      endsLeft_ $
        parseCmd ["notes", "export-folder"]

  describe "noteBasename" $ do
    it "plain ASCII title produces a hyphenated lowercase slug" $
      noteBasename "Shopping List" `shouldBe` "shopping-list"
    it "punctuation becomes hyphens with consecutive runs collapsed" $
      noteBasename "Hello, World!!!" `shouldBe` "hello-world"
    it "leading and trailing non-alphanumeric characters are stripped" $
      noteBasename "  --Title--  " `shouldBe` "title"
    it "empty title returns untitled" $
      noteBasename "" `shouldBe` "untitled"
    it "non-ASCII letters pass isAlphaNum and are preserved casefolded" $
      noteBasename "Café" `shouldBe` "café"

  describe "uniqueBasenames" $ do
    it "single Nothing title yields [untitled]" $
      uniqueBasenames [Nothing] `shouldBe` ["untitled"]
    it "all distinct titles are returned unchanged" $
      uniqueBasenames [Just "Alpha", Just "Beta"] `shouldBe` ["alpha", "beta"]
    it "duplicate title: first keeps bare slug, second gets -2" $
      uniqueBasenames [Just "foo", Just "foo"] `shouldBe` ["foo", "foo-2"]
    it "three identical titles: bare, -2, -3" $
      uniqueBasenames [Just "foo", Just "foo", Just "foo"] `shouldBe` ["foo", "foo-2", "foo-3"]
    it "natural slug collides with a suffix-generated one" $
      uniqueBasenames [Just "foo", Just "foo", Just "foo-2"]
        `shouldBe` ["foo", "foo-2", "foo-2-2"]
    it "suffix-generated slug collides with an earlier natural slug" $
      uniqueBasenames [Just "foo-2", Just "foo", Just "foo"]
        `shouldBe` ["foo-2", "foo", "foo-3"]
    it "mix of Nothing and Just titles with untitled collision" $
      uniqueBasenames [Nothing, Nothing, Just "untitled"]
        `shouldBe` ["untitled", "untitled-2", "untitled-3"]

  describe "resolveExportDest" $ do
    it "ExportFolderOutput returns the directory unchanged" $
      resolveExportDest (Just (ExportFolderOutput "/tmp/out")) "any folder"
        `shouldReturn` "/tmp/out"
    it "ExportFolderRoot appends the folder slug to the root" $
      resolveExportDest (Just (ExportFolderRoot "/data")) "My Notes"
        `shouldReturn` "/data/my-notes"
    it "ExportFolderRoot uses untitled slug when name slugs to empty" $
      resolveExportDest (Just (ExportFolderRoot "/data")) "!!!"
        `shouldReturn` "/data/untitled"

  describe "findFolderByName" $ do
    it "returns Just FolderId on an exact-case match" $
      findFolderByName "Work" testFolders `shouldBe` Just (FolderId "Folder/WORK")

    it "returns Just FolderId on a case-insensitive match" $
      findFolderByName "work" testFolders `shouldBe` Just (FolderId "Folder/WORK")

    it "returns Nothing when no folder matches" $
      findFolderByName "Missing" testFolders `shouldBe` Nothing

    it "returns Nothing for a folder whose name is absent" $
      findFolderByName "Work" [NoteFolder (FolderId "Folder/UNNAMED") Nothing] `shouldBe` Nothing


testFolders :: [NoteFolder]
testFolders =
  [ NoteFolder (FolderId "Folder/WORK") (Just "Work")
  , NoteFolder (FolderId "Folder/PERSONAL") (Just "Personal")
  ]
