{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Network.HStratus.Internal.Notes.Proto
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Protobuf decoders for the Apple Notes wire schema.

Uses 'Data.ProtocolBuffers' with Generic-derived 'Decode' instances for the
wire layer ('Wire*' types), then converts to plain domain types for use in
"Network.HStratus.Internal.Notes.Decode". Fields absent in the wire bytes
default to zero, empty, or 'Nothing', matching proto3 semantics.

The 'Wire*' types are exported so that the test-suite encoder
('HStratus.Notes.TestHelper') can construct fixture bytes via 'Encode'
instances without depending on @proto3-wire@.
-}
module Network.HStratus.Internal.Notes.Proto
  ( -- * Decoded note types
    ProtoNote (..)
  , ProtoAttributeRun (..)
  , ProtoParagraphStyle (..)
  , decodeNoteStoreProto

    -- * Wire types
  , WireNoteStoreProto (..)
  , WireDocument (..)
  , WireNote (..)
  , WireAttributeRun (..)
  , WireParagraphStyle (..)
  , WireChecklist (..)
  , WireAttachmentInfo (..)
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.ProtocolBuffers
  ( Decode
  , Encode
  , Message
  , Optional
  , Repeated
  , Value
  , decodeMessage
  , getField
  )
import Data.Serialize (runGet)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import GHC.Generics (Generic)


------------------------------------------------------------------------
-- Wire types (Generic-derived Encode/Decode)
------------------------------------------------------------------------

-- | Outermost @NoteStoreProto@ message; field 2 holds the nested 'WireDocument'.
data WireNoteStoreProto = WireNoteStoreProto
  { wnspDocument :: Optional 2 (Message WireDocument)
  }
  deriving (Generic, Show)


instance Encode WireNoteStoreProto


instance Decode WireNoteStoreProto


-- | @Document@ message nested inside 'WireNoteStoreProto'; field 3 holds the 'WireNote'.
data WireDocument = WireDocument
  { wdNote :: Optional 3 (Message WireNote)
  }
  deriving (Generic, Show)


instance Encode WireDocument


instance Decode WireDocument


-- | @Note@ message; field 2 is the note text, field 5 is the attribute runs.
data WireNote = WireNote
  { wnNoteText :: Optional 2 (Value Text)
  -- ^ full plain-text content (proto field 2)
  , wnAttributeRuns :: Repeated 5 (Message WireAttributeRun)
  -- ^ attribute runs describing formatting (proto field 5)
  }
  deriving (Generic, Show)


instance Encode WireNote


instance Decode WireNote


-- | @AttributeRun@ message describing a span of formatted text.
data WireAttributeRun = WireAttributeRun
  { warLength :: Optional 1 (Value Int32)
  -- ^ number of UTF-16 code units this run covers
  , warParagraphStyle :: Optional 2 (Message WireParagraphStyle)
  -- ^ paragraph-level formatting; absent when the run carries none
  , warFontWeight :: Optional 5 (Value Int32)
  -- ^ font weight: 1=bold, 2=italic, 3=bold+italic; absent means no weight
  , warUnderlined :: Optional 6 (Value Int32)
  -- ^ non-zero when underlined
  , warStrikethrough :: Optional 7 (Value Int32)
  -- ^ non-zero when strikethrough
  , warLink :: Optional 9 (Value Text)
  -- ^ hyperlink URL; absent means no link
  , warAttachmentInfo :: Optional 12 (Message WireAttachmentInfo)
  -- ^ attachment sub-message; absent when no attachment
  }
  deriving (Generic, Show)


instance Encode WireAttributeRun


instance Decode WireAttributeRun


-- | @ParagraphStyle@ message describing paragraph-level formatting.
data WireParagraphStyle = WireParagraphStyle
  { wpsStyleType :: Optional 1 (Value Int32)
  -- ^ 0=title, 1=heading, 2=subheading, 4=monospaced, 100=bullet, 101=dash, 102=numbered, 103=checklist
  , wpsIndentAmount :: Optional 4 (Value Int32)
  -- ^ indent level; absent means zero
  , wpsChecklist :: Optional 5 (Message WireChecklist)
  -- ^ checklist sub-message; absent when not a checklist paragraph
  , wpsListStart :: Optional 7 (Value Int32)
  -- ^ starting list item number; absent or zero means no explicit start
  , wpsBlockQuote :: Optional 8 (Value Int32)
  -- ^ non-zero when this paragraph is a block quote
  }
  deriving (Generic, Show)


instance Encode WireParagraphStyle


instance Decode WireParagraphStyle


-- | @Checklist@ sub-message (field 5 of 'WireParagraphStyle').
data WireChecklist = WireChecklist
  { wclDone :: Optional 2 (Value Int32)
  -- ^ non-zero when the checklist item is checked
  }
  deriving (Generic, Show)


instance Encode WireChecklist


instance Decode WireChecklist


-- | @AttachmentInfo@ sub-message (field 12 of 'WireAttributeRun').
data WireAttachmentInfo = WireAttachmentInfo
  { waiAttachmentId :: Optional 1 (Value Text)
  -- ^ attachment identifier string
  }
  deriving (Generic, Show)


instance Encode WireAttachmentInfo


instance Decode WireAttachmentInfo


------------------------------------------------------------------------
-- Plain domain types (unchanged; consumed by Decode.hs)
------------------------------------------------------------------------

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
  -- ^ indent_amount field 4; 0 when absent
  , ppsChecked :: Maybe Bool
  -- ^ checklist.done field 5 sub-field 2; 'Nothing' when checklist sub-message absent
  , ppsListStart :: Maybe Int32
  -- ^ starting_list_item_number field 7; 'Nothing' when absent or zero
  , ppsBlockQuote :: Bool
  -- ^ block_quote field 8 non-zero
  }
  deriving (Eq, Show)


------------------------------------------------------------------------
-- Decoding
------------------------------------------------------------------------

{- | Decode a decompressed protobuf 'ByteString' into a 'ProtoNote'.
Returns 'Left' with a message if the outer document or note field is absent,
indicating a malformed or empty payload.
-}
decodeNoteStoreProto :: ByteString -> Either String ProtoNote
decodeNoteStoreProto bs = do
  outer <- runGet decodeMessage bs
  doc <-
    maybe
      (Left "NoteStoreProto: document field absent")
      Right
      (getField (wnspDocument outer))
  wNote <-
    maybe
      (Left "Document: note field absent")
      Right
      (getField (wdNote doc))
  pure (toProtoNote wNote)


toProtoNote :: WireNote -> ProtoNote
toProtoNote wn =
  ProtoNote
    { pnNoteText = fromMaybe mempty (getField (wnNoteText wn))
    , pnAttributeRuns = map toProtoAttributeRun (getField (wnAttributeRuns wn))
    }


toProtoAttributeRun :: WireAttributeRun -> ProtoAttributeRun
toProtoAttributeRun war =
  ProtoAttributeRun
    { parLength = fromMaybe 0 (getField (warLength war))
    , parParagraphStyle = fmap toParagraphStyle (getField (warParagraphStyle war))
    , parFontWeight = fromMaybe 0 (getField (warFontWeight war))
    , parUnderlined = fromMaybe 0 (getField (warUnderlined war))
    , parStrikethrough = fromMaybe 0 (getField (warStrikethrough war))
    , parAttachmentId =
        getField (warAttachmentInfo war) >>= \ai ->
          let t = maybe LT.empty LT.fromStrict (getField (waiAttachmentId ai))
           in if LT.null t then Nothing else Just t
    , parLink = maybe LT.empty LT.fromStrict (getField (warLink war))
    }


toParagraphStyle :: WireParagraphStyle -> ProtoParagraphStyle
toParagraphStyle wps =
  ProtoParagraphStyle
    { ppsStyleType = fromMaybe 0 (getField (wpsStyleType wps))
    , ppsIndent = fromMaybe 0 (getField (wpsIndentAmount wps))
    , ppsChecked =
        fmap
          (\cl -> fromMaybe 0 (getField (wclDone cl)) /= 0)
          (getField (wpsChecklist wps))
    , ppsListStart =
        getField (wpsListStart wps) >>= \n ->
          if n == 0 then Nothing else Just n
    , ppsBlockQuote = fromMaybe 0 (getField (wpsBlockQuote wps)) /= 0
    }
