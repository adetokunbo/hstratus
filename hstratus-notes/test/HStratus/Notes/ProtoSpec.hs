{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : HStratus.Notes.ProtoSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Tests for the proto3-wire decoders in
'Network.HStratus.Internal.Notes.Proto'.
-}
module HStratus.Notes.ProtoSpec (spec) where

import Data.ByteString (ByteString)
import HStratus.Notes.Arbitraries ()
import HStratus.Notes.TestHelper
import Network.HStratus.Internal.Notes.Proto
import Test.Hspec
import Test.Hspec.Benri (endsLeft_, endsRight)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, (===))


spec :: Spec
spec = describe "decodeNoteStoreProto" $ do
  it "decodes a minimal note with text only" $
    pure (decodeNoteStoreProto minimalNoteBytes)
      `endsRight` ProtoNote{pnNoteText = "hello", pnAttributeRuns = []}

  it "returns an error for empty input" $
    endsLeft_ $
      pure (decodeNoteStoreProto "")

  it "decodes attribute run length and paragraph style" $
    case decodeNoteStoreProto noteWithRunBytes of
      Left err -> expectationFailure err
      Right note -> do
        pnNoteText note `shouldBe` "hi"
        case pnAttributeRuns note of
          [run] -> do
            parLength run `shouldBe` 2
            parParagraphStyle run
              `shouldBe` Just
                ProtoParagraphStyle
                  { ppsStyleType = 1
                  , ppsIndent = 0
                  , ppsChecked = Nothing
                  , ppsListStart = Nothing
                  , ppsBlockQuote = False
                  }
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes strikethrough field 7" $
    case decodeNoteStoreProto noteWithStrikethroughBytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] -> do
            parLength run `shouldBe` 2
            parStrikethrough run `shouldBe` 1
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes indent_amount field 4 into ppsIndent" $
    case decodeNoteStoreProto bulletIndent1Bytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsIndent (parParagraphStyle run) `shouldBe` Just 1
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes checklist.done = 1 into ppsChecked = Just True" $
    case decodeNoteStoreProto checklistDoneBytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsChecked (parParagraphStyle run) `shouldBe` Just (Just True)
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes checklist.done = 0 into ppsChecked = Just False" $
    case decodeNoteStoreProto checklistUndoneBytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsChecked (parParagraphStyle run) `shouldBe` Just (Just False)
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes absent checklist into ppsChecked = Nothing" $
    case decodeNoteStoreProto checklistAbsentBytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsChecked (parParagraphStyle run) `shouldBe` Just Nothing
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes starting_list_item_number into ppsListStart = Just 3" $
    case decodeNoteStoreProto numberedListStart3Bytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsListStart (parParagraphStyle run) `shouldBe` Just (Just 3)
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  it "decodes block_quote into ppsBlockQuote = True" $
    case decodeNoteStoreProto blockQuoteBytes of
      Left err -> expectationFailure err
      Right note ->
        case pnAttributeRuns note of
          [run] ->
            fmap ppsBlockQuote (parParagraphStyle run) `shouldBe` Just True
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)

  prop "encode/decode roundtrip preserves ProtoParagraphStyle" $ \ps ->
    case decodeNoteStoreProto (mkNote "x" [runWith 1 (encodeParagraphStyle ps)]) of
      Left err -> counterexample err False
      Right note -> case pnAttributeRuns note of
        [run] -> parParagraphStyle run === Just ps
        runs -> counterexample ("expected 1 run, got " <> show (length runs)) False


minimalNoteBytes :: ByteString
minimalNoteBytes = mkNote "hello" []


noteWithRunBytes :: ByteString
noteWithRunBytes = mkNote "hi" [runWith 2 (psStyleType 1)]


noteWithStrikethroughBytes :: ByteString
noteWithStrikethroughBytes = mkNote "hi" [runFields 2 [(7, 1)]]


bulletIndent1Bytes :: ByteString
bulletIndent1Bytes = mkNote "hi" [runWith 1 (psStyleType 100 <> psIndentAmount 1)]


checklistDoneBytes :: ByteString
checklistDoneBytes = mkNote "hi" [runWith 1 (psStyleType 103 <> psChecklist True)]


checklistUndoneBytes :: ByteString
checklistUndoneBytes = mkNote "hi" [runWith 1 (psStyleType 103 <> psChecklist False)]


checklistAbsentBytes :: ByteString
checklistAbsentBytes = mkNote "hi" [runWith 1 (psStyleType 103)]


numberedListStart3Bytes :: ByteString
numberedListStart3Bytes = mkNote "hi" [runWith 1 (psStyleType 102 <> psListStart 3)]


blockQuoteBytes :: ByteString
blockQuoteBytes = mkNote "hi" [runWith 1 psBlockQuote]
