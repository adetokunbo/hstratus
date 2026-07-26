{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.ProtoSpec (spec) where

import Data.ByteString (ByteString)
import Network.HStratus.Internal.Notes.Proto
import Test.Hspec
import Test.Hspec.Benri (endsLeft_, endsRight)


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


-- NoteStoreProto { document: Document { note: Note { note_text: "hello" } } }
--
-- Note bytes    (field 2, wire 2): 0x12 0x05 "hello"              (7 bytes)
-- Document bytes(field 3, wire 2): 0x1a 0x07 <Note>               (9 bytes)
-- Proto bytes   (field 2, wire 2): 0x12 0x09 <Document>           (11 bytes)
minimalNoteBytes :: ByteString
minimalNoteBytes = "\x12\x09\x1a\x07\x12\x05hello"


-- NoteStoreProto { document: Document { note: Note {
--   note_text: "hi",
--   attribute_run: [ AttributeRun { length: 2, paragraph_style: ParagraphStyle { style_type: 1 } } ]
-- } } }
--
-- ParagraphStyle bytes (field 1, wire 0): 0x08 0x01              (2 bytes)
-- AttributeRun bytes:
--   field 1 (length=2, wire 0): 0x08 0x02
--   field 2 (paragraph_style, wire 2): 0x12 0x02 <ParagraphStyle>
--   total: 6 bytes
-- Note bytes (field 2: "hi", field 5: AttributeRun):
--   field 2 (text "hi", wire 2): 0x12 0x02 "hi"
--   field 5 (run, wire 2): 0x2a 0x06 <AttributeRun>
--   total: 12 bytes
-- Document bytes (field 3, wire 2): 0x1a 0x0c <Note>             (14 bytes)
-- Proto bytes   (field 2, wire 2): 0x12 0x0e <Document>          (16 bytes)
noteWithRunBytes :: ByteString
noteWithRunBytes =
  "\x12\x0e\x1a\x0c\x12\x02hi\x2a\x06\x08\x02\x12\x02\x08\x01"


-- NoteStoreProto { ... attribute_run: [ AttributeRun { length: 2, strikethrough: 1 } ] }
--
-- AttributeRun: field 1 (length=2): 0x08 0x02; field 7 (strikethrough=1): 0x38 0x01
-- Note: field 2 "hi" + field 5 (run, len 4): 10 bytes
-- Document bytes (field 3, wire 2): 0x1a 0x0a <Note>             (12 bytes)
-- Proto bytes   (field 2, wire 2): 0x12 0x0c <Document>          (14 bytes)
noteWithStrikethroughBytes :: ByteString
noteWithStrikethroughBytes =
  "\x12\x0c\x1a\x0a\x12\x02hi\x2a\x04\x08\x02\x38\x01"


-- ParagraphStyle { style_type: 100 (bullet), indent_amount: 1 }
-- ParagraphStyle: 0x08 0x64 0x20 0x01 (4 bytes)
-- AttributeRun (length=1, para=PS): 8 bytes; Note 13 bytes; Doc 15 bytes; Proto 17 bytes
bulletIndent1Bytes :: ByteString
bulletIndent1Bytes =
  "\x12\x10\x1a\x0e\x12\x02hi\x2a\x08\x08\x01\x12\x04\x08\x64\x20\x01"


-- ParagraphStyle { style_type: 103 (checklist), checklist { done: 1 } }
-- Checklist sub-msg: 0x10 0x01 (done=1, field 2 wire 0)
-- ParagraphStyle: 0x08 0x67 0x2a 0x02 0x10 0x01 (6 bytes)
-- AttributeRun (length=1, para=PS): 10 bytes; Note 16 bytes; Doc 18 bytes; Proto 20 bytes
checklistDoneBytes :: ByteString
checklistDoneBytes =
  "\x12\x12\x1a\x10\x12\x02hi\x2a\x0a\x08\x01\x12\x06\x08\x67\x2a\x02\x10\x01"


-- ParagraphStyle { style_type: 103 (checklist), checklist { done: 0 } }
checklistUndoneBytes :: ByteString
checklistUndoneBytes =
  "\x12\x12\x1a\x10\x12\x02hi\x2a\x0a\x08\x01\x12\x06\x08\x67\x2a\x02\x10\x00"


-- ParagraphStyle { style_type: 103 (checklist) } — no checklist sub-message
-- ParagraphStyle: 0x08 0x67 (2 bytes)
-- AttributeRun (length=1, para=PS): 6 bytes; Note 12 bytes; Doc 14 bytes; Proto 16 bytes
checklistAbsentBytes :: ByteString
checklistAbsentBytes =
  "\x12\x0e\x1a\x0c\x12\x02hi\x2a\x06\x08\x01\x12\x02\x08\x67"


-- ParagraphStyle { style_type: 102 (numbered), starting_list_item_number: 3 }
-- ParagraphStyle: 0x08 0x66 0x38 0x03 (4 bytes; field 7 wire 0: tag=0x38)
-- AttributeRun: 8 bytes; Note 13 bytes; Doc 15 bytes; Proto 17 bytes
numberedListStart3Bytes :: ByteString
numberedListStart3Bytes =
  "\x12\x10\x1a\x0e\x12\x02hi\x2a\x08\x08\x01\x12\x04\x08\x66\x38\x03"


-- ParagraphStyle { block_quote: 1 } — style_type absent (defaults to 0)
-- ParagraphStyle: 0x40 0x01 (field 8 wire 0: tag=0x40)
-- AttributeRun (length=1, para=PS): 6 bytes; Note 12 bytes; Doc 14 bytes; Proto 16 bytes
blockQuoteBytes :: ByteString
blockQuoteBytes =
  "\x12\x0e\x1a\x0c\x12\x02hi\x2a\x06\x08\x01\x12\x02\x40\x01"
