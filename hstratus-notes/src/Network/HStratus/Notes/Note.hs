{- |
Module      : Network.HStratus.Notes.Note
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Re-exports the core iCloud Notes domain types: identifiers, summaries,
folders, note content, and inline-run styles.
-}
module Network.HStratus.Notes.Note
  ( NoteId (..)
  , FolderId (..)
  , NoteSummary (..)
  , NoteFolder (..)
  , Note (..)
  , NoteText (..)
  , NoteRun (..)
  , NoteStyle (..)
  )
where

import Network.HStratus.Internal.Notes.Note
  ( FolderId (..)
  , Note (..)
  , NoteFolder (..)
  , NoteId (..)
  , NoteRun (..)
  , NoteStyle (..)
  , NoteSummary (..)
  , NoteText (..)
  )

