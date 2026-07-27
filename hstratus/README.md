# hstratus — unified command-line tool for iCloud services

`hstratus` provides a single executable with subcommands for iCloud
authentication, Drive, and Notes.  It uses Apple ID credentials stored on disk
and depends on [`hstratus-auth`](../hstratus-auth/#readme) for the authentication
flow.


## Warning — use at your own risk

- This tool is **unofficial** and not supported by Apple.
- The iCloud APIs it uses are undocumented and may change without notice.


## Getting started

Run `hstratus auth init` once to save your Apple ID and password, then
`hstratus auth login` to authenticate.  The session token is cached on disk and
reused by the drive and notes subcommands until it expires.

```
$ hstratus auth init
Apple ID: your-apple-id@example.com
Password:
Credentials saved.

$ hstratus auth login
Authenticated.
```


## Commands

```
Usage: hstratus COMMAND

  hstratus: iCloud service tools

Available commands:
  auth   iCloud authentication commands
  drive  iCloud Drive commands
  notes  iCloud Notes commands
```

### `hstratus auth`

```
Usage: hstratus auth COMMAND

Available commands:
  init   Save Apple ID credentials to the config directory
  login  Authenticate with iCloud
```

#### `hstratus auth init`

Prompts for an Apple ID and password and saves them to
`$XDG_CONFIG_HOME/hstratus/credentials.json`.

#### `hstratus auth login`

```
Usage: hstratus auth login [--china] [--log] [--log-file FILE] [--redact]

Available options:
  --china          Use mainland China endpoints
  --log            Append HTTP exchanges to the default log file
  --log-file FILE  Append HTTP exchanges to FILE
  --redact         Redact sensitive headers (tokens, cookies) in the log
```

Runs the full sign-in flow interactively, prompting for a 2FA or 2SA
verification code when required.


### `hstratus drive`

```
Usage: hstratus drive COMMAND

Available commands:
  ls  List contents of a Drive folder (default: root)
  cp  Download a file from Drive to the local filesystem
```

#### `hstratus drive ls`

```
Usage: hstratus drive ls [[PATH]] [--china] [--log] [--log-file FILE]
                         [--log-bodies] [--redact]

  [PATH]  Slash-separated path from root (e.g. Documents/Work)
```

With no argument, lists the root folder.  With a path, lists that folder.

```
$ hstratus drive ls
FOLDER  Desktop
FOLDER  Documents
FILE    notes.txt  (1024 bytes)

$ hstratus drive ls Documents/Work
FOLDER  Archive
FILE    report.pdf  (204800 bytes)
```

#### `hstratus drive cp`

```
Usage: hstratus drive cp PATH [--root DIR | --output FILE]
                         [--china] [--log] [--log-file FILE]
                         [--log-bodies] [--redact]

  PATH           Slash-separated path to the file in Drive
  --root DIR     Copy under DIR, mirroring the Drive path
  --output FILE  Copy to the exact local path FILE
```

Without `--root` or `--output`, the file is placed under `~/icloud-drive/`
mirroring the Drive path.

```
$ hstratus drive cp Documents/report.pdf --output /tmp/report.pdf
Downloaded to /tmp/report.pdf
```


### `hstratus notes`

```
Usage: hstratus notes COMMAND

Available commands:
  list-note-folders  List all iCloud Notes folders
  list-notes         List notes, optionally filtered by folder name
  get                Fetch and display a note body (default format: markdown)
  export-folder      Download all notes in a folder to local files
```

#### `hstratus notes list-note-folders`

```
Usage: hstratus notes list-note-folders [--china] [--log] [--log-file FILE]
                                        [--log-bodies] [--redact]
```

Lists all Notes folders, showing each folder's ID and name.

#### `hstratus notes list-notes`

```
Usage: hstratus notes list-notes [--folder NAME] [--china] [--log]
                                 [--log-file FILE] [--log-bodies] [--redact]

  --folder NAME  Folder name (e.g. TukTuk)
```

Lists notes sorted by modification time.  Pass `--folder` to restrict output to
a single folder.

#### `hstratus notes get`

```
Usage: hstratus notes get NOTE_ID [--format FORMAT] [--china] [--log]
                          [--log-file FILE] [--log-bodies] [--redact]

  NOTE_ID           UUID record name, as shown by list-notes
  --format FORMAT   Output format: markdown (default) or text
```

Fetches the named note and prints its title, modification time, and body.
`--format markdown` renders the body as Markdown; `--format text` emits the
raw plain-text content.

#### `hstratus notes export-folder`

```
Usage: hstratus notes export-folder FOLDER [--root DIR | --output DIR]
                                    [--format FORMAT] [--china] [--log]
                                    [--log-file FILE] [--log-bodies] [--redact]

  FOLDER           Folder name (case-insensitive)
  --root DIR       Save under DIR/<folder-slug>/
  --output DIR     Save directly into DIR
  --format FORMAT  Output format: markdown (default) or text
```

Downloads every note in the named folder and writes each one to a local file.
Without `--root` or `--output`, files are placed under `~/icloud-notes/<folder-slug>/`.
Locked notes are skipped with a message; network errors abort the run.

```
$ hstratus notes export-folder TukTuk --output /tmp/tuktuk
saved /tmp/tuktuk/shopping-list.md
saved /tmp/tuktuk/meeting-notes.md
skipped ABC-123: note is locked
```


## Common options

All `drive` and `notes` subcommands accept these options:

| Option | Description |
|--------|-------------|
| `--china` | Use mainland China endpoints |
| `--log` | Append HTTP exchanges to the default log file |
| `--log-file FILE` | Append HTTP exchanges to FILE |
| `--log-bodies` | Include request bodies in the log |
| `--redact` | Redact tokens and cookies in the log |


---

Apple and the Apple logo are trademarks of Apple Inc., registered in the U.S. and other countries and regions.
iCloud is a service mark of Apple Inc., registered in the U.S. and other countries and regions.
