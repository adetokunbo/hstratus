{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Network.HStratus.Internal.Notes.CloudKit
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

CloudKit JSON response types for the Notes CloudKit endpoints, including
records, assets, zone-change responses, and query responses.
-}
module Network.HStratus.Internal.Notes.CloudKit
  ( CKZoneId (..)
  , CKRecordRef (..)
  , CKAsset (..)
  , CKTimestamp (..)
  , CKField (..)
  , CKRecord (..)
  , CKQueryResponse (..)
  , CKLookupResponse (..)
  , CKZoneChangesZone (..)
  , CKZoneChangesResponse (..)
  , parseMillisTimestamp
  )
where

import Control.Monad (guard)
import Data.Aeson
  ( FromJSON (..)
  , Object
  , Value
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- | Convert a millisecond-precision POSIX timestamp to 'UTCTime'.
parseMillisTimestamp :: Int64 -> UTCTime
parseMillisTimestamp ms = posixSecondsToUTCTime (fromIntegral ms / 1000)


-- | CloudKit zone identifier.
data CKZoneId = CKZoneId
  { czName :: Text
  -- ^ zone name (e.g. @Notes@)
  , czType :: Text
  -- ^ zone type (e.g. @REGULAR_CUSTOM_ZONE@)
  }
  deriving (Eq, Show)


instance FromJSON CKZoneId where
  parseJSON = withObject "CKZoneId" $ \o ->
    CKZoneId
      <$> o .: "zoneName"
      <*> o .: "zoneType"


-- | A reference to another CloudKit record.
data CKRecordRef = CKRecordRef
  { rrRecordName :: Text
  -- ^ @recordName@ of the referenced record
  , rrAction :: Text
  -- ^ referential integrity action (e.g. @NONE@, @DELETE_SELF@)
  }
  deriving (Eq, Show)


instance FromJSON CKRecordRef where
  parseJSON = withObject "CKRecordRef" $ \o ->
    CKRecordRef
      <$> o .: "recordName"
      <*> o .: "action"


-- | A CloudKit asset: an encrypted file attachment associated with a record.
data CKAsset = CKAsset
  { caDownloadUrl :: Text
  -- ^ pre-signed URL from which the asset content can be downloaded
  , caFileChecksum :: Text
  -- ^ SHA-256 checksum of the encrypted asset content
  , caRefChecksum :: Text
  -- ^ reference checksum used when committing an upload
  , caWrappingKey :: Text
  -- ^ encryption wrapping key for the asset
  , caSize :: Int64
  -- ^ byte size of the encrypted asset content
  }
  deriving (Eq, Show)


instance FromJSON CKAsset where
  parseJSON = withObject "CKAsset" $ \o ->
    CKAsset
      <$> o .: "downloadURL"
      <*> o .: "fileChecksum"
      <*> o .: "referenceChecksum"
      <*> o .: "wrappingKey"
      <*> o .: "size"


-- | A CloudKit creation or modification timestamp with the responsible user.
data CKTimestamp = CKTimestamp
  { ctTimestamp :: Int64
  -- ^ millisecond-precision POSIX timestamp
  , ctUserRecordName :: Text
  -- ^ @recordName@ of the user who created or last modified the record
  }
  deriving (Eq, Show)


instance FromJSON CKTimestamp where
  parseJSON = withObject "CKTimestamp" $ \o ->
    CKTimestamp
      <$> o .: "timestamp"
      <*> o .: "userRecordName"


-- Internal newtypes: give distinct Haskell types to CK tags that share a
-- primitive (Text covers "STRING"/"ENCRYPTED_BYTES"; Int64 covers
-- "INT64"/"TIMESTAMP"), making CKFieldTag a total function.
newtype CKString = CKString Text
  deriving (FromJSON)


newtype CKEncryptedBytes = CKEncryptedBytes Text
  deriving (FromJSON)


newtype CKInt64Value = CKInt64Value Int64
  deriving (FromJSON)


newtype CKTimestampValue = CKTimestampValue Int64
  deriving (FromJSON)


-- Single source of truth mapping each value type to its CloudKit "type" tag.
type family CKFieldTag a :: Symbol where
  CKFieldTag CKString = "STRING"
  CKFieldTag CKInt64Value = "INT64"
  CKFieldTag CKTimestampValue = "TIMESTAMP"
  CKFieldTag CKEncryptedBytes = "ENCRYPTED_BYTES"
  CKFieldTag CKRecordRef = "REFERENCE"
  CKFieldTag [CKRecordRef] = "REFERENCE_LIST"
  CKFieldTag CKAsset = "ASSETID"


-- Confirms the pre-parsed "type" tag matches CKFieldTag a, then parses "value".
matchField
  :: forall a
   . (KnownSymbol (CKFieldTag a), FromJSON a)
  => Text
  -> Object
  -> Parser a
matchField typ o = do
  guard (typ == pack (symbolVal (Proxy :: Proxy (CKFieldTag a))))
  o .: "value"


-- | A single typed field value in a CloudKit record.
data CKField
  = -- | a plain text (@STRING@) field value
    CKStringField Text
  | -- | a 64-bit integer (@INT64@) field value
    CKInt64Field Int64
  | -- | a millisecond POSIX timestamp (@TIMESTAMP@) field value
    CKTimestampField Int64
  | -- | an encrypted bytes (@ENCRYPTED_BYTES@) field, base64-encoded
    CKEncryptedBytesField Text
  | -- | a single record reference (@REFERENCE@) field value
    CKReferenceField CKRecordRef
  | -- | a list of record references (@REFERENCE_LIST@) field value
    CKReferenceListField [CKRecordRef]
  | -- | an asset identifier (@ASSETID@) field value
    CKAssetIdField CKAsset
  | -- | a field with an unrecognised type tag and its raw JSON value
    CKUnknownField Text Value
  deriving (Eq, Show)


instance FromJSON CKField where
  parseJSON = withObject "CKField" $ \o -> do
    typ <- o .: "type" :: Parser Text
    asum
      [ CKStringField . (\(CKString t) -> t) <$> matchField typ o
      , CKInt64Field . (\(CKInt64Value i) -> i) <$> matchField typ o
      , CKTimestampField . (\(CKTimestampValue i) -> i) <$> matchField typ o
      , CKEncryptedBytesField . (\(CKEncryptedBytes t) -> t) <$> matchField typ o
      , CKReferenceField <$> matchField typ o
      , CKReferenceListField <$> matchField typ o
      , CKAssetIdField <$> matchField typ o
      , CKUnknownField typ <$> o .: "value"
      ]


-- | A CloudKit record with its name, type, fields, and metadata.
data CKRecord = CKRecord
  { crName :: Text
  -- ^ unique identifier for the record within its zone
  , crType :: Maybe Text
  -- ^ record type (e.g. @Note@, @Folder@); @Nothing@ in delete tombstones
  , crChangeTag :: Maybe Text
  -- ^ opaque version tag used for conflict detection
  , crZoneId :: Maybe CKZoneId
  -- ^ zone containing this record; @Nothing@ in some response shapes
  , crFields :: Map Text CKField
  -- ^ typed field values keyed by field name; empty when the record is deleted
  , crCreated :: Maybe CKTimestamp
  -- ^ creation timestamp and user; @Nothing@ when absent
  , crModified :: Maybe CKTimestamp
  -- ^ last-modification timestamp and user; @Nothing@ when absent
  , crDeleted :: Maybe Bool
  -- ^ @Just True@ for delete tombstones; @Nothing@ otherwise
  }
  deriving (Eq, Show)


instance FromJSON CKRecord where
  parseJSON = withObject "CKRecord" $ \o ->
    CKRecord
      <$> o .: "recordName"
      <*> o .:? "recordType"
      <*> o .:? "recordChangeTag"
      <*> o .:? "zoneID"
      <*> o .:? "fields" .!= mempty
      <*> o .:? "created"
      <*> o .:? "modified"
      <*> o .:? "deleted"


-- | Response body for a CloudKit record query.
data CKQueryResponse = CKQueryResponse
  { qrRecords :: [CKRecord]
  -- ^ records returned by the query
  , qrContinuationMarker :: Maybe Value
  -- ^ pagination cursor; @Nothing@ when all results fit in one response
  }
  deriving (Eq, Show)


instance FromJSON CKQueryResponse where
  parseJSON = withObject "CKQueryResponse" $ \o ->
    CKQueryResponse
      <$> o .: "records"
      <*> o .:? "continuationMarker"


-- | Response body for a CloudKit record lookup by name.
data CKLookupResponse = CKLookupResponse
  { lrRecords :: [CKRecord]
  -- ^ records returned by the lookup, in the same order as the request
  , lrSyncToken :: Maybe Text
  -- ^ sync token for subsequent zone-changes requests; @Nothing@ when absent
  }
  deriving (Eq, Show)


instance FromJSON CKLookupResponse where
  parseJSON = withObject "CKLookupResponse" $ \o ->
    CKLookupResponse
      <$> o .: "records"
      <*> o .:? "syncToken"


-- | Per-zone section of a CloudKit zone-changes response.
data CKZoneChangesZone = CKZoneChangesZone
  { zczZoneId :: CKZoneId
  -- ^ identifier of the zone these changes belong to
  , zczSyncToken :: Maybe Text
  -- ^ opaque token for the next zone-changes request for this zone
  , zczMoreComing :: Maybe Bool
  -- ^ @Just True@ when further pages of changes remain for this zone
  , zczRecords :: [CKRecord]
  -- ^ records that changed (or were deleted) in this zone
  }
  deriving (Eq, Show)


instance FromJSON CKZoneChangesZone where
  parseJSON = withObject "CKZoneChangesZone" $ \o ->
    CKZoneChangesZone
      <$> o .: "zoneID"
      <*> o .:? "syncToken"
      <*> o .:? "moreComing"
      <*> o .:? "records" .!= []


-- | Top-level response body for a CloudKit zone-changes request.
newtype CKZoneChangesResponse = CKZoneChangesResponse
  { zcrZones :: [CKZoneChangesZone]
  -- ^ per-zone change sets included in this response
  }
  deriving (Eq, Show)


instance FromJSON CKZoneChangesResponse where
  parseJSON = withObject "CKZoneChangesResponse" $ \o ->
    CKZoneChangesResponse <$> o .: "zones"
