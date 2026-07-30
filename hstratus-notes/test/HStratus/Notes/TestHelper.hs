{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : HStratus.Notes.TestHelper
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Programmatic proto\/gzip fixture builders for 'ProtoSpec' and 'DecodeSpec':
encodes domain values with @protobuf@ and compresses with @zlib@, avoiding
hand-crafted byte literals.
-}
module HStratus.Notes.TestHelper
  ( mkNote
  , mkNoteGzip
  , mkNoteZlib
  , mkEmptyGzip
  , runWith
  , plainRun
  , strikethroughRun
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
import Data.ProtocolBuffers (encodeMessage, putField)
import Data.Serialize (runPut)
import Data.Text (Text)
import Network.HStratus.Internal.Notes.Note
  ( NoteRun (..)
  , NoteStyle (..)
  , NoteText (..)
  )
import Network.HStratus.Internal.Notes.Proto
  ( ProtoParagraphStyle (..)
  , WireAttachmentInfo (..)
  , WireAttributeRun (..)
  , WireChecklist (..)
  , WireDocument (..)
  , WireNote (..)
  , WireNoteStoreProto (..)
  , WireParagraphStyle (..)
  )


{- | Encode a NoteStoreProto with the given note text and attribute runs.
Returns raw (uncompressed) proto bytes.
-}
mkNote :: Text -> [WireAttributeRun] -> ByteString
mkNote noteText runs =
  runPut . encodeMessage $
    WireNoteStoreProto
      { wnspDocument =
          putField . Just $
            WireDocument
              { wdNote =
                  putField . Just $
                    WireNote
                      { wnNoteText = putField (Just noteText)
                      , wnAttributeRuns = putField runs
                      }
              }
      }


-- | Like 'mkNote' but gzip-compressed, suitable for 'decodeNoteBody'.
mkNoteGzip :: Text -> [WireAttributeRun] -> ByteString
mkNoteGzip noteText runs =
  LBS.toStrict . GZip.compress . LBS.fromStrict $ mkNote noteText runs


-- | Like 'mkNote' but zlib-compressed, suitable for 'decodeNoteBody'.
mkNoteZlib :: Text -> [WireAttributeRun] -> ByteString
mkNoteZlib noteText runs =
  LBS.toStrict . Zlib.compress . LBS.fromStrict $ mkNote noteText runs


{- | Gzip-compressed empty proto (no document field).
'decodeNoteBody' returns @Left@ for this input.
-}
mkEmptyGzip :: ByteString
mkEmptyGzip = LBS.toStrict (GZip.compress LBS.empty)


-- | An 'WireAttributeRun' with the given length and a 'WireParagraphStyle' sub-message.
runWith :: Int32 -> WireParagraphStyle -> WireAttributeRun
runWith len style =
  WireAttributeRun
    { warLength = putField (Just len)
    , warParagraphStyle = putField (Just style)
    , warFontWeight = putField Nothing
    , warUnderlined = putField Nothing
    , warStrikethrough = putField Nothing
    , warLink = putField Nothing
    , warAttachmentInfo = putField Nothing
    }


{- | A 'WireAttributeRun' with the given length and all other fields absent.
Use record-update syntax to set individual scalar fields for targeted tests.
-}
plainRun :: Int32 -> WireAttributeRun
plainRun len =
  WireAttributeRun
    { warLength = putField (Just len)
    , warParagraphStyle = putField Nothing
    , warFontWeight = putField Nothing
    , warUnderlined = putField Nothing
    , warStrikethrough = putField Nothing
    , warLink = putField Nothing
    , warAttachmentInfo = putField Nothing
    }


-- | A 'WireAttributeRun' of the given length with the strikethrough field set.
strikethroughRun :: Int32 -> WireAttributeRun
strikethroughRun len = (plainRun len){warStrikethrough = putField (Just 1)}


-- ParagraphStyle constructors and modifiers -----------------------------------

-- | A 'WireParagraphStyle' with only the @style_type@ field set.
psStyleType :: Int32 -> WireParagraphStyle
psStyleType n =
  WireParagraphStyle
    { wpsStyleType = putField (Just n)
    , wpsIndentAmount = putField Nothing
    , wpsChecklist = putField Nothing
    , wpsListStart = putField Nothing
    , wpsBlockQuote = putField Nothing
    }


-- | Set the @indent_amount@ field on a 'WireParagraphStyle'.
psIndentAmount :: Int32 -> WireParagraphStyle -> WireParagraphStyle
psIndentAmount n ps = ps{wpsIndentAmount = putField (Just n)}


-- | Set the @checklist@ sub-message on a 'WireParagraphStyle'.
psChecklist :: Bool -> WireParagraphStyle -> WireParagraphStyle
psChecklist done ps =
  ps
    { wpsChecklist =
        putField . Just $
          WireChecklist{wclDone = putField (Just (if done then 1 else 0))}
    }


-- | Set the @starting_list_item_number@ field on a 'WireParagraphStyle'.
psListStart :: Int32 -> WireParagraphStyle -> WireParagraphStyle
psListStart n ps = ps{wpsListStart = putField (Just n)}


-- | A 'WireParagraphStyle' with only the @block_quote@ field set.
psBlockQuote :: WireParagraphStyle
psBlockQuote =
  WireParagraphStyle
    { wpsStyleType = putField Nothing
    , wpsIndentAmount = putField Nothing
    , wpsChecklist = putField Nothing
    , wpsListStart = putField Nothing
    , wpsBlockQuote = putField (Just 1)
    }


-- Encoder functions -----------------------------------------------------------

{- | Encode a 'ProtoParagraphStyle' as a 'WireParagraphStyle'.
Proto3 default values (0 / False / Nothing) are omitted to match wire convention.
-}
encodeParagraphStyle :: ProtoParagraphStyle -> WireParagraphStyle
encodeParagraphStyle ProtoParagraphStyle{ppsStyleType, ppsIndent, ppsChecked, ppsListStart, ppsBlockQuote} =
  WireParagraphStyle
    { wpsStyleType = putField (if ppsStyleType /= 0 then Just ppsStyleType else Nothing)
    , wpsIndentAmount = putField (if ppsIndent /= 0 then Just ppsIndent else Nothing)
    , wpsChecklist =
        putField $
          fmap
            (\c -> WireChecklist{wclDone = putField (Just (if c then 1 else 0))})
            ppsChecked
    , wpsListStart = putField ppsListStart
    , wpsBlockQuote = putField (if ppsBlockQuote then Just 1 else Nothing)
    }


{- | Encode a 'NoteStyle' as a 'WireParagraphStyle'.
Inverse of 'toNoteStyle'; 'StyleBody False' is not in the image of
'toNoteStyle' and should not appear in generated test data.
-}
encodeNoteStyle :: NoteStyle -> WireParagraphStyle
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


-- | Encode a 'NoteRun' as a 'WireAttributeRun'.
encodeNoteRun :: NoteRun -> WireAttributeRun
encodeNoteRun NoteRun{nrLength, nrStyle, nrBold, nrItalic, nrUnderline, nrStrikethrough, nrAttachmentId, nrLink} =
  WireAttributeRun
    { warLength = putField (Just nrLength)
    , warParagraphStyle = putField (fmap encodeNoteStyle nrStyle)
    , warFontWeight = putField (if fw /= 0 then Just fw else Nothing)
    , warUnderlined = putField (if nrUnderline then Just 1 else Nothing)
    , warStrikethrough = putField (if nrStrikethrough then Just 1 else Nothing)
    , warLink = putField nrLink
    , warAttachmentInfo = putField (fmap mkAttachmentInfo nrAttachmentId)
    }
 where
  fw :: Int32
  fw = case (nrBold, nrItalic) of
    (True, True) -> 3
    (True, False) -> 1
    (False, True) -> 2
    (False, False) -> 0
  mkAttachmentInfo t = WireAttachmentInfo{waiAttachmentId = putField (Just t)}


-- | Encode a 'NoteText' to gzip-compressed proto bytes, suitable for 'decodeNoteBody'.
encodeNoteText :: NoteText -> ByteString
encodeNoteText NoteText{ntText, ntRuns} = mkNoteGzip ntText (map encodeNoteRun ntRuns)
