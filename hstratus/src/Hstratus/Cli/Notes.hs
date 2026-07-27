{-# LANGUAGE LambdaCase #-}
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
  , GetFormat (..)
  , ExportFolderDest (..)
  , noteBasename
  , uniqueBasenames
  , resolveExportDest
  , notesParser
  , runNotes
  , findFolderByName
  )
where

import Control.Exception (catch)
import Control.Monad ((>=>))
import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, onServiceError, runWithApi)
import Network.HStratus.Notes
import Network.HStratus.Notes.Markdown (noteToMarkdown)
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))


-- | Top-level Notes subcommand.
data NotesCommand
  = -- | list all Notes folders
    NotesListFolders !CommonOpts
  | -- | list notes, optionally filtered by folder name
    NotesListNotes !ListNotesOpts
  | -- | fetch and display the body of a note by ID
    NotesGet !GetOpts
  deriving (Eq, Show)


-- | Options for the @notes list-notes@ subcommand.
data ListNotesOpts = ListNotesOpts
  { lnFolder :: !(Maybe Text)
  -- ^ optional folder name to filter by; @Nothing@ lists recent notes across all folders
  , lnCommon :: !CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Output format for the @notes get@ subcommand.
data GetFormat
  = -- | render the note body as Markdown (default)
    GetMarkdown
  | -- | emit the raw plain-text content of the note
    GetText
  deriving (Eq, Show)


-- | Options for the @notes get@ subcommand.
data GetOpts = GetOpts
  { gnNoteId :: !NoteId
  -- ^ UUID record name of the note to fetch
  , gnFormat :: !GetFormat
  -- ^ output format; defaults to 'GetMarkdown'
  , gnCommon :: !CommonOpts
  -- ^ shared connection and logging options
  }
  deriving (Eq, Show)


-- | Destination specifier for the @notes export-folder@ subcommand.
data ExportFolderDest
  = -- | write files under @DIR\/\<folder-slug\>\/@
    ExportFolderRoot !FilePath
  | -- | write files directly into @DIR@ (no slug appended)
    ExportFolderOutput !FilePath
  deriving (Eq, Show)


{- | Map a note title to a filesystem-safe slug.

Applies 'Data.Char.isAlphaNum' as a filter (non-alphanumeric characters become
@\'-\'@), collapses consecutive hyphens, strips leading and trailing hyphens,
and case-folds the result.  Returns @\"untitled\"@ for titles that produce an
empty slug.
-}
noteBasename :: Text -> Text
noteBasename t =
  let mapped = Text.map (\c -> if isAlphaNum c then c else '-') (Text.toCaseFold t)
      parts = filter (not . Text.null) (Text.splitOn "-" mapped)
      slug = Text.intercalate "-" parts
   in if Text.null slug then "untitled" else slug


{- | Allocate a unique slug for each note title, preserving list order.

@Nothing@ titles use @\"untitled\"@ as the base slug.  When a slug collides
with one already allocated earlier in the list, a numeric suffix is appended
(@\"-2\"@, @\"-3\"@, …) until the candidate is unique.  The first occurrence
of a slug always keeps the bare form.
-}
uniqueBasenames :: [Maybe Text] -> [Text]
uniqueBasenames = go Set.empty
 where
  go _ [] = []
  go seen (mt : rest) =
    let base = maybe "untitled" noteBasename mt
        slug = findUnique seen base
     in slug : go (Set.insert slug seen) rest
  findUnique seen base
    | Set.notMember base seen = base
    | otherwise = findSuffix seen base 2
  findSuffix seen base n =
    let candidate = base <> "-" <> Text.pack (show n)
     in if Set.notMember candidate seen
          then candidate
          else findSuffix seen base (n + 1)


{- | Resolve the local output directory for a folder export, expanding @~@ via
'System.Directory.getHomeDirectory' for the default case.

* 'ExportFolderOutput' @dir@ — returns @dir@ unchanged.
* 'ExportFolderRoot' @root@ — appends the slug of @folderName@ to @root@.
* 'Nothing' — uses @~\/icloud-notes\/\<slug\>@ as the default.
-}
resolveExportDest :: Maybe ExportFolderDest -> Text -> IO FilePath
resolveExportDest (Just (ExportFolderOutput dir)) _ = pure dir
resolveExportDest (Just (ExportFolderRoot root)) folderName =
  pure $ root </> Text.unpack (noteBasename folderName)
resolveExportDest Nothing folderName = do
  home <- getHomeDirectory
  pure $ home </> "icloud-notes" </> Text.unpack (noteBasename folderName)


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
              (progDesc "Fetch and display a note body (default format: markdown)")
          )
    )


getOptsParser :: Parser GetOpts
getOptsParser =
  (GetOpts . NoteId . Text.pack <$> argument str (metavar "NOTE_ID" <> help "UUID record name (e.g. 68567409-5528-458C-9A00-7A2AB485CAD6), as shown by list-notes"))
    <*> option
      ( eitherReader $ \s -> case s of
          "markdown" -> Right GetMarkdown
          "text" -> Right GetText
          _ -> Left ("unknown format: " <> s <> "; use markdown or text")
      )
      ( long "format"
          <> metavar "FORMAT"
          <> value GetMarkdown
          <> showDefault
          <> help "Output format: markdown (default) or text"
      )
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
runListFolders opts = withNotesApi opts (noteFolders >=> mapM_ printFolder)


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
    let body = case gnFormat opts of
          GetMarkdown -> noteToMarkdown nt
          GetText -> ntText nt
    putStrLn (Text.unpack body)


resolveFolderName :: NotesApi -> Text -> IO FolderId
resolveFolderName na name = do
  findFolderByName name <$> noteFolders na >>= \case
    Just fid -> pure fid
    Nothing -> do
      putStrLn $ "No folder named '" <> Text.unpack name <> "'"
      exitFailure


-- | Find the first folder whose name matches the given string (case-insensitive); returns its 'FolderId'.
findFolderByName :: Text -> [NoteFolder] -> Maybe FolderId
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
