{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.HStratus.Internal.Notes.Download
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

HTTP fetchers for the iCloud Notes CloudKit endpoints: recent notes, folders,
notes in a folder, and individual note lookup.
-}
module Network.HStratus.Internal.Notes.Download
  ( NotesError (..)
  , fetchFolders
  , fetchRecent
  , fetchNote
  , fetchNotesInFolder
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe, mapMaybe)
import Network.HStratus.Http (Api, HStratusError, rawRequest)
import Network.HStratus.Internal.Notes.CloudKit
  ( CKLookupResponse (..)
  , CKQueryResponse (..)
  , CKZoneChangesResponse (..)
  , CKZoneChangesZone (..)
  )
import Network.HStratus.Internal.Notes.Endpoints
  ( NotesEndpoints
  , changesBody
  , changesReq
  , foldersBody
  , lookupBody
  , lookupReq
  , queryReq
  , recentsBody
  )
import Network.HStratus.Internal.Notes.Note
  ( FolderId
  , Note
  , NoteFolder
  , NoteId (..)
  , NoteSummary (..)
  )
import Network.HStratus.Internal.Notes.NoteData
  ( noteRecordToNote
  , parseFoldersFromQuery
  , parseSummariesFromChanges
  , parseSummariesFromQuery
  )
import Network.HTTP.Client
  ( Request
  , RequestBody (..)
  , Response (..)
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Types (hContentType, statusCode)


-- | Errors that can occur during iCloud Notes API calls.
data NotesError
  = -- | the server returned an unexpected HTTP status code
    NotesHttpError Int
  | -- | a JSON response could not be decoded into the expected structure
    NotesParseError String


instance Show NotesError where
  show (NotesHttpError n) = "iCloud Notes: HTTP error " <> show n
  show (NotesParseError msg) = "iCloud Notes: parse error: " <> msg


instance Exception NotesError


instance HStratusError NotesError


-- | Fetch all note folders, following continuation markers to retrieve every page.
fetchFolders :: Api -> NotesEndpoints -> IO [NoteFolder]
fetchFolders api ep = go Nothing []
 where
  go marker acc = do
    qr <- fetchAs "fetchFolders" api (jsonReq (foldersBody 200 marker) (queryReq ep))
    let acc' = acc <> parseFoldersFromQuery qr
    case qrContinuationMarker qr of
      Nothing -> pure acc'
      Just m -> go (Just m) acc'


-- | Fetch recent note summaries, following continuation markers to retrieve every page.
fetchRecent :: Api -> NotesEndpoints -> IO [NoteSummary]
fetchRecent api ep = go Nothing []
 where
  go marker acc = do
    qr <- fetchAs "fetchRecent" api (jsonReq (recentsBody 200 marker) (queryReq ep))
    let acc' = acc <> parseSummariesFromQuery qr
    case qrContinuationMarker qr of
      Nothing -> pure acc'
      Just m -> go (Just m) acc'


-- | Fetch a single note by its 'NoteId'; returns @Nothing@ if the record is absent or cannot be decoded.
fetchNote :: Api -> NotesEndpoints -> NoteId -> IO (Maybe Note)
fetchNote api ep nid = do
  lr <- fetchAs "fetchNote" api (jsonReq (lookupBody [unNoteId nid]) (lookupReq ep))
  pure $ listToMaybe $ mapMaybe noteRecordToNote (lrRecords lr)


-- | Fetch all non-deleted note summaries belonging to the given folder, paging through zone changes.
fetchNotesInFolder :: Api -> NotesEndpoints -> FolderId -> IO [NoteSummary]
fetchNotesInFolder api ep fid = go Nothing []
 where
  go mToken acc = do
    cr <- fetchAs "fetchNotesInFolder" api (jsonReq (changesBody mToken) (changesReq ep))
    let acc' = acc <> filter inFolder (parseSummariesFromChanges cr)
    case nextToken cr of
      Nothing -> pure acc'
      Just tok -> go (Just tok) acc'
  inFolder s = nsFolderId s == Just fid && not (nsDeleted s)
  nextToken cr = case zcrZones cr of
    (z : _) | zczMoreComing z == Just True -> zczSyncToken z
    _ -> Nothing


fetchAs :: (FromJSON a) => String -> Api -> Request -> IO a
fetchAs ctx api r = rawRequest' api r >>= decodeAs ctx


rawRequest' :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest' api r = do
  resp <- rawRequest api r
  checkStatus resp
  pure resp


jsonReq :: LBS.ByteString -> Request -> Request
jsonReq body r =
  r
    { requestBody = RequestBodyLBS body
    , requestHeaders = (hContentType, "application/json") : requestHeaders r
    }


checkStatus :: Response a -> IO ()
checkStatus resp =
  let code = statusCode (responseStatus resp)
   in when (code >= 400) $ throwIO (NotesHttpError code)


decodeAs :: (FromJSON a) => String -> Response LBS.ByteString -> IO a
decodeAs ctx resp =
  case eitherDecode (responseBody resp) of
    Left err -> throwIO (NotesParseError (ctx <> ": " <> err))
    Right v -> pure v
