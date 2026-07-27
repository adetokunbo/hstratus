{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.NotesSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Notes
  ( GetFormat (..)
  , GetOpts (..)
  , ListNotesOpts (..)
  , NotesCommand (..)
  , findFolderByName
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
