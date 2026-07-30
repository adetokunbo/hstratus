{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hstratus.Cli.CommonSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Tests for the shared CLI options parser in 'Hstratus.Cli.Common'.
-}
module Hstratus.Cli.CommonSpec (spec) where

import Hstratus.Cli.Common (commonOptsParser)
import Network.HStratus.Http.Cli (CommonOpts (..))
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, fullDesc, info, renderFailure)
import Test.Hspec
import Test.Hspec.Benri (endsRight)


defaultOpts :: CommonOpts
defaultOpts = CommonOpts False False Nothing False False


spec :: Spec
spec = describe "Hstratus.Cli.Common.commonOptsParser" $ do
  it "defaults all flags to False with no log file" $
    pure (parseOpts []) `endsRight` defaultOpts

  it "sets optChina when --china is given" $
    fmap optChina (parseOpts ["--china"]) `shouldBe` Right True

  it "sets optLog when --log is given" $
    fmap optLog (parseOpts ["--log"]) `shouldBe` Right True

  it "sets optLogFile when --log-file is given" $
    fmap optLogFile (parseOpts ["--log-file", "/tmp/test.log"])
      `shouldBe` Right (Just "/tmp/test.log")

  it "sets optLogBodies when --log-bodies is given" $
    fmap optLogBodies (parseOpts ["--log-bodies"]) `shouldBe` Right True

  it "sets optRedact when --redact is given" $
    fmap optRedact (parseOpts ["--redact"]) `shouldBe` Right True

  it "accepts multiple flags together" $
    pure (parseOpts ["--log", "--redact", "--china"])
      `endsRight` defaultOpts{optChina = True, optLog = True, optRedact = True}


parseOpts :: [String] -> Either String CommonOpts
parseOpts args =
  case execParserPure defaultPrefs (info commonOptsParser fullDesc) args of
    Success opts -> Right opts
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"
