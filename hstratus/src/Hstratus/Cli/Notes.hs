{-# LANGUAGE TypeApplications #-}

{- |
Module      : Hstratus.Cli.Notes
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

CLI subcommands for iCloud Notes (list, get, render).
-}
module Hstratus.Cli.Notes
  ( NotesCommand (..)
  , ListNotesOpts (..)
  , GetOpts (..)
  , notesParser
  , runNotes
  , findFolderByName
  )
where

import Control.Exception (catch)
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, onServiceError, runWithApi)
import Network.HStratus.Notes
import Options.Applicative
import System.Exit (die, exitFailure)


-- | Top-level Notes subcommand.
data NotesCommand
  = -- | list all Notes folders
    NotesListFolders CommonOpts
  | -- | list notes, optionally filtered by folder name
    NotesListNotes ListNotesOpts
  | -- | fetch and display the body of a note by ID
    NotesGet GetOpts
  deriving (Eq, Show)


-- | Options for the @notes list-notes@ subcommand.
data ListNotesOpts = ListNotesOpts
  { lnFolder :: Maybe Text.Text
  -- ^ optional folder name to filter by; @Nothing@ lists recent notes across all folders
  , lnCommon :: CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Options for the @notes get@ subcommand.
data GetOpts = GetOpts
  { gnNoteId :: NoteId
  -- ^ UUID record name of the note to fetch
  , gnCommon :: CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Optparse-applicative parser for the @notes@ subcommand.
notesParser :: Parser NotesCommand
notesParser =
  subparser
    ( command
        "list-note-folders"
        ( info
            (NotesListFolders <$> commonOptsParser <**> helper)
            (progDesc "List all iCloud Notes folders")
        )
        <> command
          "list-notes"
          ( info
              (NotesListNotes <$> listNotesOptsParser <**> helper)
              (progDesc "List notes, optionally filtered by folder name")
          )
        <> command
          "get"
          ( info
              (NotesGet <$> getOptsParser <**> helper)
              (progDesc "Fetch and display the plain-text body of a note")
          )
    )


getOptsParser :: Parser GetOpts
getOptsParser =
  GetOpts
    <$> (NoteId . Text.pack <$> argument str (metavar "NOTE_ID" <> help "UUID record name (e.g. 68567409-5528-458C-9A00-7A2AB485CAD6), as shown by list-notes"))
    <*> commonOptsParser


listNotesOptsParser :: Parser ListNotesOpts
listNotesOptsParser =
  ListNotesOpts
    <$> optional
      ( Text.pack
          <$> strOption
            ( long "folder"
                <> metavar "NAME"
                <> help "Folder name (e.g. TukTuk)"
            )
      )
    <*> commonOptsParser


-- | Dispatch a 'NotesCommand' to its handler.
runNotes :: NotesCommand -> IO ()
runNotes (NotesListFolders opts) = runListFolders opts
runNotes (NotesListNotes opts) = runListNotes opts
runNotes (NotesGet opts) = runGet opts


runListFolders :: CommonOpts -> IO ()
runListFolders opts =
  withNotesApi opts $ \na ->
    noteFolders na >>= mapM_ printFolder


runListNotes :: ListNotesOpts -> IO ()
runListNotes opts =
  withNotesApi (lnCommon opts) $ \na -> do
    notes <- case lnFolder opts of
      Nothing -> recentNotes na
      Just name -> do
        fid <- resolveFolderName na name
        notesInFolder na fid
    mapM_ printNote notes


runGet :: GetOpts -> IO ()
runGet opts =
  withNotesApi (gnCommon opts) $ \na -> do
    let nid = gnNoteId opts
    mnote <- getNote na nid
    note <- case mnote of
      Nothing -> die $ "Note not found: " <> Text.unpack (unNoteId nid)
      Just n -> pure n
    result <- decodeNoteBody (noteBodyBytes note)
    nt <- case result of
      Left err -> die $ "Failed to decode note body: " <> err
      Right decoded -> pure decoded
    let s = noteInfo note
        titleStr = maybe "Untitled" Text.unpack (nsTitle s)
    mapM_ putStrLn $
      catMaybes
        [ Just (titleStr <> "  [" <> Text.unpack (unNoteId nid) <> "]")
        , fmap (\t -> "Modified: " <> show t) (nsModified s)
        , Just ""
        ]
    putStrLn (Text.unpack (ntText nt))


resolveFolderName :: NotesApi -> Text.Text -> IO FolderId
resolveFolderName na name = do
  folders <- noteFolders na
  case findFolderByName name folders of
    Just fid -> pure fid
    Nothing -> do
      putStrLn $ "No folder named '" <> Text.unpack name <> "'"
      exitFailure


-- | Find the first folder whose name matches the given string (case-insensitive); returns its 'FolderId'.
findFolderByName :: Text.Text -> [NoteFolder] -> Maybe FolderId
findFolderByName name = fmap nfId . find matchesName
 where
  matchesName nf = maybe False (\fn -> Text.toCaseFold fn == Text.toCaseFold name) (nfName nf)


printFolder :: NoteFolder -> IO ()
printFolder nf =
  putStrLn $ Text.unpack (unFolderId (nfId nf)) <> nameStr
 where
  nameStr = maybe "" (("  " <>) . Text.unpack) (nfName nf)


printNote :: NoteSummary -> IO ()
printNote ns =
  putStrLn $ Text.unpack (unNoteId (nsId ns)) <> titleStr
 where
  titleStr = maybe "" (("  " <>) . Text.unpack) (nsTitle ns)


withNotesApi :: CommonOpts -> (NotesApi -> IO ()) -> IO ()
withNotesApi opts runAction =
  runWithApi opts (\ad sess api -> mkNotesApi ad sess api >>= runAction)
    `catch` onServiceError @NotesError
