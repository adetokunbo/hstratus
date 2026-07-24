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


## CLI usage

A command-line interface using this behaviour is provided by the [`hstratus`](../hstratus/#readme)
package.  Use [`hstratus notes list-note-folders`](../hstratus#hstratus-notes-list-note-folders)
to list folders and [`hstratus notes list-notes`](../hstratus#hstratus-notes-list-notes)
to list notes (optionally filtered by folder name).
