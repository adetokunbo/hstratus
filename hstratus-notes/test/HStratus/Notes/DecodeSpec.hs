{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.DecodeSpec (spec) where

import Data.ByteString (ByteString)
import Network.HStratus.Internal.Notes.Decode (decodeNoteBody)
import Network.HStratus.Internal.Notes.Note (NoteRun (..), NoteText (..))
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
        , ntRuns =
            [ NoteRun
                { nrLength = 2
                , nrStyle = Nothing
                , nrBold = False
                , nrItalic = False
                , nrUnderline = False
                , nrStrikethrough = True
                , nrLink = Nothing
                }
            ]
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


-- gzip( NoteStoreProto {} ) — valid gzip, but the document field is absent,
-- so decodeNoteBody should return Left "NoteStoreProto: document field absent".
emptyNoteBytes :: ByteString
emptyNoteBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\x03\x00\x00\x00\x00\x00\x00\x00\
  \\x00\x00"
