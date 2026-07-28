# hstratus — Haskell client for iCloud services

`hstratus` is a set of Haskell libraries and a CLI for accessing iCloud services using an Apple ID.

## Packages

| Package | Description |
|---------|-------------|
| [`hstratus`](hstratus/#readme) | Unified command-line tool for iCloud services |
| [`hstratus-auth`](hstratus-auth/#readme) | Authenticate with iCloud using Apple ID credentials |
| [`hstratus-drive`](hstratus-drive/#readme) | Browse, download, and mutate files in iCloud Drive |
| [`hstratus-notes`](hstratus-notes/#readme) | Browse, fetch, and export notes and folders from iCloud Notes |

`hstratus` is the unified CLI, with subcommands for authentication, Drive, and Notes.  The
`hstratus-drive` and `hstratus-notes` packages both depend on `hstratus-auth` for authentication.


## Disclaimer — use at your own risk

- These libraries are **unofficial** and not supported by Apple.
- The iCloud APIs they use are undocumented, and may change or cease functioning without notice.


## Credits

The iCloud API behaviour documented and implemented here has benefitted immensely from the
work put into these earlier projects:

- [icloudpy](https://github.com/mandarons/icloudpy) — Python iCloud client
- [pyicloud](https://github.com/timlaing/pyicloud) — Python iCloud client, a fork of `icloudpy`
- [fastlane](https://github.com/fastlane/fastlane) — iOS/macOS automation tools, whose
  Spaceship library implements iCloud authentication


## License

BSD-3-Clause


---

Apple and the Apple logo are trademarks of Apple Inc., registered in the U.S. and other countries and regions.
iCloud is a service mark of Apple Inc., registered in the U.S. and other countries and regions.
