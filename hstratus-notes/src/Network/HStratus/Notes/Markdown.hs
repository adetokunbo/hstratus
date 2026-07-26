{- |
Module      : Network.HStratus.Notes.Markdown
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Re-exports 'noteToMarkdown' for converting a decoded note body to Markdown text.
-}
module Network.HStratus.Notes.Markdown
  ( noteToMarkdown
  )
where

import Network.HStratus.Internal.Notes.Markdown
  ( noteToMarkdown
  )

