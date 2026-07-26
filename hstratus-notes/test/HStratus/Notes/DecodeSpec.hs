{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.DecodeSpec (spec) where

import Data.ByteString (ByteString)
import Network.HStratus.Internal.Notes.Decode (decodeNoteBody)
import Network.HStratus.Internal.Notes.Note (NoteRun (..), NoteStyle (..), NoteText (..))
import Test.Hspec
import Test.Hspec.Benri (endsLeft_, endsRight)


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
    , nrLink = Nothing
    }


-- gzip( NoteStoreProto { document=2: Document { note=3: Note { note_text=2: "Step 6b test" } } } )
-- Generated with mtime=0 for determinism.
-- Proto hex: 12101a0e120c537465702036622074657374
fixtureBytes :: ByteString
fixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\x13\x12\x90\xe2\x13\xe2\x09\x2e\
  \\x49\x2d\x50\x30\x4b\x52\x28\x49\x2d\x2e\x01\x00\x41\xcb\xcc\x34\x12\x00\
  \\x00\x00"


-- gzip( NoteStoreProto { document{ note{ note_text="hi",
--   attribute_run{ length=2, strikethrough=1 } } } } )
-- Proto hex: 120c1a0a120268692a0408023801
strikethroughFixtureBytes :: ByteString
strikethroughFixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\xe2\x91\xe2\x12\x62\
  \\xca\xc8\xd4\x62\xe1\x60\xb2\x60\x04\x00\xf3\x5e\xcf\x2d\x0e\x00\
  \\x00\x00"


-- gzip( ... attribute_run{ length=1, paragraph_style{ style_type=100, indent_amount=1 } } )
-- Proto hex: 12101a0e120268692a080801120408642001
bulletIndent1FixtureBytes :: ByteString
bulletIndent1FixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\x12\x90\xe2\x13\x62\
  \\xca\xc8\xd4\xe2\xe0\x60\x14\x62\xe1\x48\x51\x60\x04\x00\xda\x79\
  \\xdf\x03\x12\x00\x00\x00"


-- gzip( ... attribute_run{ length=1, paragraph_style{ style_type=103, checklist{ done=1 } } } )
-- Proto hex: 12121a10120268692a0a0801120608672a021001
checklistDoneFixtureBytes :: ByteString
checklistDoneFixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\x12\x92\x12\x10\x62\
  \\xca\xc8\xd4\xe2\xe2\x60\x14\x62\xe3\x48\xd7\x62\x12\x60\x04\x00\
  \\xf5\x3c\x7b\xe9\x14\x00\x00\x00"


-- gzip( ... attribute_run{ length=1, paragraph_style{ style_type=103, checklist{ done=0 } } } )
-- Proto hex: 12121a10120268692a0a0801120608672a021000
checklistUndoneFixtureBytes :: ByteString
checklistUndoneFixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\x12\x92\x12\x10\x62\
  \\xca\xc8\xd4\xe2\xe2\x60\x14\x62\xe3\x48\xd7\x62\x12\x60\x00\x00\
  \\x63\x0c\x7c\x9e\x14\x00\x00\x00"


-- gzip( ... attribute_run{ length=1, paragraph_style{ style_type=102, starting_list_item_number=3 } } )
-- Proto hex: 12101a0e120268692a080801120408663803
numberedListStart3FixtureBytes :: ByteString
numberedListStart3FixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\x12\x90\xe2\x13\x62\
  \\xca\xc8\xd4\xe2\xe0\x60\x14\x62\xe1\x48\xb3\x60\x06\x00\xc1\x54\
  \\x4e\x6c\x12\x00\x00\x00"


-- gzip( ... attribute_run{ length=1, paragraph_style{ block_quote=1 } } )
-- Proto hex: 120e1a0c120268692a06080112024001
blockQuoteFixtureBytes :: ByteString
blockQuoteFixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03\x13\xe2\x93\xe2\x11\x62\
  \\xca\xc8\xd4\x62\xe3\x60\x14\x62\x72\x60\x04\x00\x98\x7a\x70\x60\
  \\x10\x00\x00\x00"


-- gzip( NoteStoreProto {} ) — valid gzip, but the document field is absent,
-- so decodeNoteBody should return Left "NoteStoreProto: document field absent".
emptyNoteBytes :: ByteString
emptyNoteBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\x03\x00\x00\x00\x00\x00\x00\x00\
  \\x00\x00"
