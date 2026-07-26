{- |
Module      : Network.HStratus.Internal.Notes.Proto
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Hand-written proto3-wire decoders for the Apple Notes protobuf schema.

Uses proto3-wire's lower-level @Parser@ API rather than code generation
because the schema has non-consecutive field numbers (e.g. @note_text = 2@,
@attribute_run = 5@); explicit @\`at\` N@ bindings prevent the silent
field-number mismatches that Generic derivation would introduce.
Fields not listed in the decoders are silently ignored by the wire decoder.
-}
module Network.HStratus.Internal.Notes.Proto
  ( ProtoNote (..)
  , ProtoAttributeRun (..)
  , ProtoParagraphStyle (..)
  , decodeNoteStoreProto
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Proto3.Wire.Decode
  ( Parser
  , RawMessage
  , at
  , embedded
  , embedded'
  , int32
  , one
  , parse
  , repeated
  , text
  )


-- Proto types mirror the schema closely; conversion to domain NoteText/NoteRun
-- types happens in Decode.hs where gzip decompression also lives.

-- | Top-level note content decoded from the protobuf payload.
data ProtoNote = ProtoNote
  { pnNoteText :: Text
  -- ^ full plain-text content of the note (proto field 2)
  , pnAttributeRuns :: [ProtoAttributeRun]
  -- ^ attribute runs describing inline and paragraph formatting (proto field 5)
  }
  deriving (Eq, Show)


-- | A single attribute run from the protobuf schema.
data ProtoAttributeRun = ProtoAttributeRun
  { parLength :: Int32
  -- ^ number of UTF-16 code units this run covers
  , parParagraphStyle :: Maybe ProtoParagraphStyle
  -- ^ paragraph-level formatting; @Nothing@ when the run has no paragraph style
  , parFontWeight :: Int32
  -- ^ font weight: 0 = none, 1 = bold, 2 = italic, 3 = bold+italic
  , parUnderlined :: Int32
  -- ^ non-zero when the run is underlined
  , parStrikethrough :: Int32
  -- ^ non-zero when the run has strikethrough
  , parAttachmentId :: Maybe LT.Text
  -- ^ attachment identifier; @Nothing@ when absent or empty in the wire bytes
  , parLink :: LT.Text
  -- ^ hyperlink URL; empty string when no link is present
  }
  deriving (Eq, Show)


-- | Paragraph-level formatting for an attribute run.
data ProtoParagraphStyle = ProtoParagraphStyle
  { ppsStyleType :: Int32
  -- ^ style_type field 1: 0=title, 1=heading, 2=subheading, 4=monospaced, 100=bullet, 101=dash, 102=numbered, 103=checklist
  , ppsIndent :: Int32
  -- ^ indent_amount field 4; 0 when absent.
  , ppsChecked :: Maybe Bool
  -- ^ checklist.done field 5 sub-field 2; 'Nothing' when checklist sub-message absent.
  , ppsListStart :: Maybe Int32
  -- ^ starting_list_item_number field 7; 'Nothing' when absent or zero.
  , ppsBlockQuote :: Bool
  -- ^ block_quote field 8 non-zero.
  }
  deriving (Eq, Show)


{- | Decode a gzip-decompressed protobuf ByteString into a 'ProtoNote'.
Returns 'Left' with a message if the outer document or note field is absent,
which would indicate a malformed or empty payload rather than a real note.
-}
decodeNoteStoreProto :: ByteString -> Either String ProtoNote
decodeNoteStoreProto bs =
  case parse parseNoteStoreProto bs of
    Left err -> Left (show err)
    Right Nothing -> Left "NoteStoreProto: document field absent"
    Right (Just Nothing) -> Left "Document: note field absent"
    Right (Just (Just note)) -> Right note


-- Drill straight through NoteStoreProto (field 2) → Document (field 3) → Note
-- without defining a separate Document record type.  Each `embedded` call wraps
-- the result in Maybe: Nothing means the field was absent in the wire bytes.
parseNoteStoreProto :: Parser RawMessage (Maybe (Maybe ProtoNote))
parseNoteStoreProto = embedded (embedded parseProtoNote `at` 3) `at` 2


-- `one text LT.empty` reads a singular string field, returning the default
-- (empty) when the field is absent.  `fmap LT.toStrict` converts the lazy
-- Text that proto3-wire produces to the strict Text used in ProtoNote.
-- `repeated (embedded' ...)` collects all occurrences of a length-delimited
-- field into a list; embedded' (vs embedded) is used inside repeated because
-- the field is known to be present (not optional) at each occurrence.
parseProtoNote :: Parser RawMessage ProtoNote
parseProtoNote =
  ProtoNote
    <$> (fmap LT.toStrict (one text LT.empty) `at` 2)
    <*> (repeated (embedded' parseProtoAttributeRun) `at` 5)


-- Scalar optional fields (font_weight, underlined, strikethrough) use
-- `one int32 0` — the proto3 default of 0 means "absent / no effect" for
-- all of them (0 = FONT_WEIGHT_UNKNOWN, 0 = not underlined, etc.).
-- `embedded` for paragraph_style returns Maybe: Nothing when the run carries
-- no paragraph-level formatting.
parseProtoAttributeRun :: Parser RawMessage ProtoAttributeRun
parseProtoAttributeRun =
  ProtoAttributeRun
    <$> (one int32 0 `at` 1)
    <*> (embedded parseParagraphStyle `at` 2)
    <*> (one int32 0 `at` 5) -- font_weight (fields 3 and 4 are absent in schema)
    <*> (one int32 0 `at` 6) -- underlined
    <*> (one int32 0 `at` 7) -- strikethrough
    <*> fmap (>>= \t -> if LT.null t then Nothing else Just t) (embedded parseAttachmentInfo `at` 12)
    <*> (one text LT.empty `at` 9) -- link (field 8 skipped: superscript)


parseAttachmentInfo :: Parser RawMessage LT.Text
parseAttachmentInfo = one text LT.empty `at` 1


parseParagraphStyle :: Parser RawMessage ProtoParagraphStyle
parseParagraphStyle =
  ProtoParagraphStyle
    <$> (one int32 0 `at` 1)
    <*> (one int32 0 `at` 4)
    <*> (embedded parseChecklist `at` 5)
    <*> (fmap (\n -> if n == 0 then Nothing else Just n) (one int32 0) `at` 7)
    <*> (fmap (/= 0) (one int32 0) `at` 8)


parseChecklist :: Parser RawMessage Bool
parseChecklist = fmap (/= 0) (one int32 0 `at` 2)
