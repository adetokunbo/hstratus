{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Network.HStratus.Internal.Notes.Decode
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Decodes a gzip-compressed protobuf note body into the 'NoteText' domain type,
bridging the wire representation in "Network.HStratus.Internal.Notes.Proto"
and the public 'NoteText', 'NoteRun', and 'NoteStyle' types.
-}
module Network.HStratus.Internal.Notes.Decode
  ( decodeNoteBody
  )
where

import qualified Codec.Compression.GZip as GZip
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.HStratus.Internal.Notes.Note
  ( NoteRun (..)
  , NoteStyle (..)
  , NoteText (..)
  )
import Network.HStratus.Internal.Notes.Proto
  ( ProtoAttributeRun (..)
  , ProtoNote (..)
  , ProtoParagraphStyle (..)
  , decodeNoteStoreProto
  )


{- | Decode a note body.  The input is the raw bytes from the CloudKit
@TextDataEncrypted@ field after base64-decoding (Phase 1 does this in
'noteRecordToNote').  The encoding is: gzip( protobuf( NoteStoreProto ) ).
Returns @Left@ if decompression fails or the protobuf cannot be parsed.
-}
decodeNoteBody :: ByteString -> IO (Either String NoteText)
decodeNoteBody bs = do
  decompressed <- try (evaluate (LBS.toStrict (GZip.decompress (LBS.fromStrict bs))))
  pure $ case (decompressed :: Either SomeException ByteString) of
    Left e -> Left ("gzip decompression failed: " <> show e)
    Right strict -> fmap toNoteText (decodeNoteStoreProto strict)


-- Map ProtoNote → NoteText.  The proto layer mirrors the wire schema; this
-- function applies the domain interpretation (e.g. font_weight int → bold/italic
-- booleans, empty link string → Nothing).
toNoteText :: ProtoNote -> NoteText
toNoteText ProtoNote{pnNoteText, pnAttributeRuns} =
  NoteText
    { ntText = pnNoteText
    , ntRuns = map toNoteRun pnAttributeRuns
    }


toNoteRun :: ProtoAttributeRun -> NoteRun
toNoteRun
  ProtoAttributeRun
    { parLength
    , parParagraphStyle
    , parFontWeight
    , parUnderlined
    , parStrikethrough
    , parAttachmentId
    , parLink
    } =
    NoteRun
      { nrLength = parLength
      , nrStyle = parParagraphStyle >>= toNoteStyle
      , -- FontWeight enum: 1=bold, 2=italic, 3=bold+italic
        nrBold = parFontWeight == 1 || parFontWeight == 3
      , nrItalic = parFontWeight == 2 || parFontWeight == 3
      , nrUnderline = parUnderlined /= 0
      , nrStrikethrough = parStrikethrough /= 0
      , nrAttachmentId = fmap LT.toStrict parAttachmentId
      , -- proto3-wire yields lazy Text; empty string means no link
        nrLink = let t = LT.toStrict parLink in if T.null t then Nothing else Just t
      }


-- StyleType enum from notes.proto.  Values not in this list represent future
-- or unknown styles; if block_quote is set they map to StyleBody True,
-- otherwise to Nothing (rendered as plain body text).
-- style_type 0 (title) defers to block_quote: a paragraph_style with only
-- block_quote set has style_type = 0 by proto3 default, so we give blockquote
-- priority over title for that case.
toNoteStyle :: ProtoParagraphStyle -> Maybe NoteStyle
toNoteStyle
  ProtoParagraphStyle
    { ppsStyleType
    , ppsIndent
    , ppsChecked
    , ppsListStart
    , ppsBlockQuote
    } =
    let indent = fromIntegral ppsIndent
        listStart = fmap fromIntegral ppsListStart
     in case ppsStyleType of
          0 -> if ppsBlockQuote then Just (StyleBody True) else Just StyleTitle
          1 -> Just StyleHeading
          2 -> Just StyleSubheading
          4 -> Just StyleMonospaced
          100 -> Just (StyleBullet indent)
          101 -> Just (StyleDash indent)
          102 -> Just (StyleNumbered indent listStart)
          103 -> Just (StyleChecklist indent (fromMaybe False ppsChecked))
          _ -> if ppsBlockQuote then Just (StyleBody True) else Nothing
