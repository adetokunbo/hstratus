{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.DecodeSpec (spec) where

import Data.ByteString (ByteString)
import HStratus.Notes.Arbitraries ()
import HStratus.Notes.TestHelper
import Network.HStratus.Internal.Notes.Decode (decodeNoteBody)
import Network.HStratus.Internal.Notes.Note (NoteRun (..), NoteStyle (..), NoteText (..))
import Test.Hspec
import Test.Hspec.Benri (endsLeft_, endsRight)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (ioProperty, (===))


spec :: Spec
spec = describe "decodeNoteBody" $ do
  it "decodes a gzip-compressed protobuf note and extracts the text" $
    decodeNoteBody fixtureBytes
      `endsRight` NoteText{ntText = "Step 6b test", ntRuns = []}

  it "returns Left for bytes that are valid gzip but empty protobuf" $
    endsLeft_ $
      decodeNoteBody emptyNoteBytes

  it "returns Left for a corrupt gzip payload" $
    endsLeft_ $
      decodeNoteBody "\x00\x01\x02\x03"

  it "propagates strikethrough to nrStrikethrough" $
    decodeNoteBody strikethroughFixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrLength = 2, nrStrikethrough = True}]
        }

  it "decodes indent_amount into StyleBullet level" $
    decodeNoteBody bulletIndent1FixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrStyle = Just (StyleBullet 1)}]
        }

  it "decodes checklist done into StyleChecklist True" $
    decodeNoteBody checklistDoneFixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrStyle = Just (StyleChecklist 0 True)}]
        }

  it "decodes checklist undone into StyleChecklist False" $
    decodeNoteBody checklistUndoneFixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrStyle = Just (StyleChecklist 0 False)}]
        }

  it "decodes starting_list_item_number into StyleNumbered list start" $
    decodeNoteBody numberedListStart3FixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrStyle = Just (StyleNumbered 0 (Just 3))}]
        }

  it "decodes block_quote into StyleBody True" $
    decodeNoteBody blockQuoteFixtureBytes
      `endsRight` NoteText
        { ntText = "hi"
        , ntRuns = [baseRun{nrStyle = Just (StyleBody True)}]
        }

  it "propagates attachment_identifier to nrAttachmentId" $
    decodeNoteBody attachmentIdFixtureBytes
      `endsRight` NoteText
        { ntText = "\xFFFC"
        , ntRuns = [baseRun{nrAttachmentId = Just "att-1"}]
        }

  prop "encode/decode roundtrip preserves NoteText" $ \nt ->
    ioProperty $ do
      result <- decodeNoteBody (encodeNoteText nt)
      pure (result === Right nt)


-- Base NoteRun with all defaults; individual tests override specific fields.
baseRun :: NoteRun
baseRun =
  NoteRun
    { nrLength = 1
    , nrStyle = Nothing
    , nrBold = False
    , nrItalic = False
    , nrUnderline = False
    , nrStrikethrough = False
    , nrAttachmentId = Nothing
    , nrLink = Nothing
    }


fixtureBytes :: ByteString
fixtureBytes = mkNoteGzip "Step 6b test" []


emptyNoteBytes :: ByteString
emptyNoteBytes = mkEmptyGzip


strikethroughFixtureBytes :: ByteString
strikethroughFixtureBytes = mkNoteGzip "hi" [runFields 2 [(7, 1)]]


bulletIndent1FixtureBytes :: ByteString
bulletIndent1FixtureBytes = mkNoteGzip "hi" [runWith 1 (psStyleType 100 <> psIndentAmount 1)]


checklistDoneFixtureBytes :: ByteString
checklistDoneFixtureBytes = mkNoteGzip "hi" [runWith 1 (psStyleType 103 <> psChecklist True)]


checklistUndoneFixtureBytes :: ByteString
checklistUndoneFixtureBytes = mkNoteGzip "hi" [runWith 1 (psStyleType 103 <> psChecklist False)]


numberedListStart3FixtureBytes :: ByteString
numberedListStart3FixtureBytes = mkNoteGzip "hi" [runWith 1 (psStyleType 102 <> psListStart 3)]


blockQuoteFixtureBytes :: ByteString
blockQuoteFixtureBytes = mkNoteGzip "hi" [runWith 1 psBlockQuote]


attachmentIdFixtureBytes :: ByteString
attachmentIdFixtureBytes = mkNoteGzip "\xFFFC" [encodeNoteRun baseRun{nrAttachmentId = Just "att-1"}]
