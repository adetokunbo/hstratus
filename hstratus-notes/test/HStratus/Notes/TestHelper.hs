{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : HStratus.Notes.TestHelper
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Programmatic proto\/gzip fixture builders for 'ProtoSpec' and 'DecodeSpec':
encodes domain values with @proto3-wire@ and compresses with @zlib@, avoiding
hand-crafted byte literals.
-}
module HStratus.Notes.TestHelper
  ( mkNote
  , mkNoteGzip
  , mkNoteZlib
  , mkEmptyGzip
  , runWith
  , runFields
  , psStyleType
  , psIndentAmount
  , psChecklist
  , psListStart
  , psBlockQuote
  , encodeParagraphStyle
  , encodeNoteStyle
  , encodeNoteRun
  , encodeNoteText
  )
where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Network.HStratus.Internal.Notes.Note
  ( NoteRun (..)
  , NoteStyle (..)
  , NoteText (..)
  )
import Network.HStratus.Internal.Notes.Proto (ProtoParagraphStyle (..))
import qualified Proto3.Wire.Encode as Encode


{- | Encode a NoteStoreProto with the given note text and attribute runs.
Returns raw (uncompressed) proto bytes.
-}
mkNote :: Text -> [Encode.MessageBuilder] -> ByteString
mkNote noteText runs =
  LBS.toStrict . Encode.toLazyByteString $
    Encode.embedded 2 $
      Encode.embedded 3 $
        Encode.text 2 (TL.fromStrict noteText)
          <> foldMap (Encode.embedded 5) runs


-- | Like 'mkNote' but gzip-compressed, suitable for 'decodeNoteBody'.
mkNoteGzip :: Text -> [Encode.MessageBuilder] -> ByteString
mkNoteGzip noteText runs =
  LBS.toStrict . GZip.compress . LBS.fromStrict $ mkNote noteText runs


-- | Like 'mkNote' but zlib-compressed, suitable for 'decodeNoteBody'.
mkNoteZlib :: Text -> [Encode.MessageBuilder] -> ByteString
mkNoteZlib noteText runs =
  LBS.toStrict . Zlib.compress . LBS.fromStrict $ mkNote noteText runs


{- | Gzip-compressed empty proto (no document field).
'decodeNoteBody' returns @Left@ for this input.
-}
mkEmptyGzip :: ByteString
mkEmptyGzip = LBS.toStrict (GZip.compress LBS.empty)


-- | An AttributeRun with the given length and a ParagraphStyle sub-message.
runWith :: Int32 -> Encode.MessageBuilder -> Encode.MessageBuilder
runWith len style = Encode.int32 1 len <> Encode.embedded 2 style


{- | An AttributeRun with the given length and a list of (fieldNumber, value)
pairs for inline fields (font_weight, underlined, strikethrough, etc.).
-}
runFields :: Int32 -> [(Int32, Int32)] -> Encode.MessageBuilder
runFields len fields =
  Encode.int32 1 len
    <> foldMap (\(fn, v) -> Encode.int32 (fromIntegral fn) v) fields


-- ParagraphStyle field builders -------------------------------------------

psStyleType :: Int32 -> Encode.MessageBuilder
psStyleType = Encode.int32 1


psIndentAmount :: Int32 -> Encode.MessageBuilder
psIndentAmount = Encode.int32 4


-- | Encode a Checklist sub-message (field 5) with the given done state.
psChecklist :: Bool -> Encode.MessageBuilder
psChecklist done =
  Encode.embedded 5 (Encode.int32 2 (if done then 1 else 0))


psListStart :: Int32 -> Encode.MessageBuilder
psListStart = Encode.int32 7


psBlockQuote :: Encode.MessageBuilder
psBlockQuote = Encode.int32 8 1


-- Encoder functions -------------------------------------------------------

{- | Encode a 'ProtoParagraphStyle' back to wire bytes, reusing the ps* helpers.
Proto3 default values (0 / False / Nothing) are omitted to match wire convention.
-}
encodeParagraphStyle :: ProtoParagraphStyle -> Encode.MessageBuilder
encodeParagraphStyle ProtoParagraphStyle{ppsStyleType, ppsIndent, ppsChecked, ppsListStart, ppsBlockQuote} =
  (if ppsStyleType /= 0 then psStyleType ppsStyleType else mempty)
    <> (if ppsIndent /= 0 then psIndentAmount ppsIndent else mempty)
    <> maybe mempty psChecklist ppsChecked
    <> maybe mempty psListStart ppsListStart
    <> (if ppsBlockQuote then psBlockQuote else mempty)


{- | Encode a 'NoteStyle' as a paragraph_style sub-message.
Inverse of 'toNoteStyle'; 'StyleBody False' is not in the image of
'toNoteStyle' and should not appear in generated test data.
-}
encodeNoteStyle :: NoteStyle -> Encode.MessageBuilder
encodeNoteStyle style = encodeParagraphStyle $ case style of
  StyleTitle -> ProtoParagraphStyle 0 0 Nothing Nothing False
  StyleHeading -> ProtoParagraphStyle 1 0 Nothing Nothing False
  StyleSubheading -> ProtoParagraphStyle 2 0 Nothing Nothing False
  StyleMonospaced -> ProtoParagraphStyle 4 0 Nothing Nothing False
  StyleBody q -> ProtoParagraphStyle 0 0 Nothing Nothing q
  StyleBullet i -> ProtoParagraphStyle 100 (fromIntegral i) Nothing Nothing False
  StyleDash i -> ProtoParagraphStyle 101 (fromIntegral i) Nothing Nothing False
  StyleNumbered i ms -> ProtoParagraphStyle 102 (fromIntegral i) Nothing (fmap fromIntegral ms) False
  StyleChecklist i c -> ProtoParagraphStyle 103 (fromIntegral i) (Just c) Nothing False


{- | Encode a 'NoteRun' as an AttributeRun sub-message.
'nrLink' is always 'Nothing' in generated runs (deferred).
-}
encodeNoteRun :: NoteRun -> Encode.MessageBuilder
encodeNoteRun NoteRun{nrLength, nrStyle, nrBold, nrItalic, nrUnderline, nrStrikethrough, nrAttachmentId, nrLink} =
  Encode.int32 1 nrLength
    <> maybe mempty (Encode.embedded 2 . encodeNoteStyle) nrStyle
    <> (if fw /= 0 then Encode.int32 5 fw else mempty)
    <> (if nrUnderline then Encode.int32 6 1 else mempty)
    <> (if nrStrikethrough then Encode.int32 7 1 else mempty)
    <> maybe mempty (\t -> Encode.embedded 12 (Encode.text 1 (TL.fromStrict t))) nrAttachmentId
    <> maybe mempty (Encode.text 9 . TL.fromStrict) nrLink
 where
  fw :: Int32
  fw = case (nrBold, nrItalic) of
    (True, True) -> 3
    (True, False) -> 1
    (False, True) -> 2
    (False, False) -> 0


-- | Encode a 'NoteText' to gzip-compressed proto bytes, suitable for 'decodeNoteBody'.
encodeNoteText :: NoteText -> ByteString
encodeNoteText NoteText{ntText, ntRuns} = mkNoteGzip ntText (map encodeNoteRun ntRuns)
