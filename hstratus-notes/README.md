# hstratus-notes — unofficial access to iCloud Notes

`hstratus-notes` reads notes and folders from iCloud Notes using an authenticated
session from [`hstratus-auth`](../hstratus-auth/).

Provides read-only access to the Notes CloudKit database: listing folders,
fetching recent notes, and downloading note content.


## Disclaimer — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud Notes API it uses is undocumented and may change without notice.


## Usage

After a successful login with `hstratus-auth`, construct a `NotesApi` value and
use it to browse notes.

### Listing recent notes

```haskell
import Network.HStratus.Http (mkApi, login, AuthState (..))
import Network.HStratus.Http.Endpoints (Realm (..))
import Network.HStratus.Notes

example :: IO ()
example = do
  api <- mkApi Usual
  result <- login api
  case result of
    Authenticated sess ad -> do
      na    <- mkNotesApi ad sess api
      notes <- recentNotes na
      mapM_ print notes
    _ -> putStrLn "Unexpected result"
```

### Listing folders

```haskell
foldersExample :: NotesApi -> IO ()
foldersExample na = do
  folders <- noteFolders na
  mapM_ print folders
```

### Listing notes in a folder

```haskell
folderNotesExample :: NotesApi -> FolderId -> IO ()
folderNotesExample na fid = do
  notes <- notesInFolder na fid
  mapM_ print notes
```

### Fetching and decoding a note body

```haskell
import qualified Data.Text.IO as TIO
import Network.HStratus.Notes
import Network.HStratus.Notes.Markdown (noteToMarkdown)

getNoteExample :: NotesApi -> NoteId -> IO ()
getNoteExample na nid = do
  mnote <- getNote na nid
  case mnote of
    Nothing   -> putStrLn "Note not found"
    Just note -> do
      result <- decodeNoteBody (noteBodyBytes note)
      case result of
        Left err -> putStrLn $ "Decode error: " <> err
        Right nt -> TIO.putStrLn (noteToMarkdown nt)
```

`getNote` returns `Nothing` when the note has been deleted since the listing
was fetched.  `decodeNoteBody` returns `Left` when the compressed protobuf
cannot be decoded.


## CLI usage

A command-line interface using this behaviour is provided by the [`hstratus`](../hstratus/#readme)
package.

| Command | Description |
|---------|-------------|
| [`hstratus notes list-note-folders`](../hstratus#hstratus-notes-list-note-folders) | List all Notes folders |
| [`hstratus notes list-notes`](../hstratus#hstratus-notes-list-notes) | List notes, optionally filtered by folder name |
| [`hstratus notes get`](../hstratus#hstratus-notes-get) | Fetch and display a note body |
| [`hstratus notes export-folder`](../hstratus#hstratus-notes-export-folder) | Download all notes in a folder to local files |


---

Apple and the Apple logo are trademarks of Apple Inc., registered in the U.S. and other countries and regions.
iCloud is a service mark of Apple Inc., registered in the U.S. and other countries and regions.
