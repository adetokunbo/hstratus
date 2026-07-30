{- |
Module      : Hstratus.Cli.Common
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Optparse-applicative parser for the options shared by all iCloud CLI subcommands.
-}
module Hstratus.Cli.Common
  ( commonOptsParser
  )
where

import Network.HStratus.Http.Cli (CommonOpts (..))
import Options.Applicative


-- | Parser for 'CommonOpts'.
commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional
      (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))
    <*> switch (long "log-bodies" <> help "Include request bodies in the HTTP exchange log")
    <*> switch (long "redact" <> help "Redact sensitive headers (tokens, cookies) in the log")
