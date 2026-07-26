{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Module      : Network.HStratus.Internal.Http
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Internal HTTP request body builders and SRP authentication context types.
-}
module Network.HStratus.Internal.Http
  ( validateSetupBody
  , phoneCodeBody
  , phoneTriggerBody
  , needsRetry
  , PasswordProtocol (..)
  , KeyDeriver (..)
  , SrpContext (..)
  , hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  )
where

import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , XCalculator (..)
  , hashMany
  , hashText
  )
import Data.Aeson (FromJSON (..), Value (..), withText)
import Data.Aeson.KeyMap (fromList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HStratus.Internal.PBKDF2 (FancyPseudoRandomF, deriveKey)
import Network.HStratus.Internal.Trust (Setup2SADevice (..), TrustedPhone (..))
import Network.HTTP.Types.Header (HeaderName)


-- | Models the known values of password protocol
data PasswordProtocol
  = -- | legacy @s2k_fo@ protocol: password is hex-encoded before hashing
    Old
  | -- | current @s2k@ protocol: password is hashed directly
    New
  deriving (Eq, Show)


instance FromJSON PasswordProtocol where
  parseJSON =
    let fromText "s2k" = Right New
        fromText "s2k_fo" = Right Old
        fromText alt = Left $ "unknown PasswordProtocol: " ++ show alt
     in withText "PasswordProtocol" $ either fail pure . fromText


-- | Data used during key derivation and verification
data KeyDeriver = KeyDeriver
  { kdTag :: !Text
  -- ^ SRP session tag from Apple's server response
  , kdIterations :: !Word64
  -- ^ PBKDF2 iteration count from Apple's server response
  , kdProtocol :: !PasswordProtocol
  -- ^ password hashing protocol in use for this session
  , kdWrappedF :: !FancyPseudoRandomF
  -- ^ PBKDF2 pseudo-random function, pre-wrapped with the negotiated hash algorithm
  }


instance XCalculator KeyDeriver where
  calcX = calcXUsingKeyDeriver


calcXUsingKeyDeriver :: KeyDeriver -> FromClient -> FromServer -> BS.ByteString
calcXUsingKeyDeriver kd fc fs =
  let FromServer{fsSalt, fsKnownAlgorithm = hashAlgo} = fs
      h = hashMany hashAlgo
      KeyDeriver{kdIterations = count, kdWrappedF, kdProtocol} = kd
      useProtocol Old = Base16.encode
      useProtocol New = id
      hashed = useProtocol kdProtocol $ hashText hashAlgo $ fcPassword fc
      reallyHashed = deriveKey kdWrappedF hashed fsSalt count
   in h [fsSalt, h [":", reallyHashed]]


-- | Bundles the SRP client\/server data and key deriver for a single auth attempt
data SrpContext = SrpContext
  { srpFromClient :: !FromClient
  -- ^ client-side SRP values (public key, password verifier input)
  , srpFromServer :: !FromServer
  -- ^ server-side SRP values (salt, public key, hash algorithm)
  , srpKeyDeriver :: !KeyDeriver
  -- ^ key derivation parameters negotiated during SRP init
  }


-- | @HeaderName@s used to capture session info from HTTP responses
hCountry, hSessionId, hSessionToken, hTrustToken, hCounter :: HeaderName
hCountry = mk "X-Apple-ID-Account-Country"
hSessionId = mk "X-Apple-ID-Session-Id"
hSessionToken = mk "X-Apple-Session-Token"
hTrustToken = mk "X-Apple-TwoSV-Trust-Token"
hCounter = mk "scnt"


-- | Build the JSON body to submit a legacy 2SA verification code for a given device.
validateSetupBody :: Setup2SADevice -> Text -> Value
validateSetupBody (Setup2SADevice fields) code =
  Object $ fields <> fromList [("verificationCode", String code), ("trustBrowser", Bool True)]


-- | Build the JSON body to trigger an SMS code to the given phone
phoneTriggerBody :: TrustedPhone -> Value
phoneTriggerBody tp =
  Object $
    fromList
      [ ("phoneNumber", Object $ fromList [("id", Number $ fromIntegral $ tpnId tp)])
      , ("mode", String $ fromMaybe "sms" $ tpnPushMode tp)
      ]


-- | @True@ when the HTTP status code warrants a single automatic retry (421, 450, or 500).
needsRetry :: Int -> Bool
needsRetry status = status == 421 || status == 450 || status == 500


-- | Build the JSON body to verify an SMS code received on the given phone
phoneCodeBody :: TrustedPhone -> Text -> Value
phoneCodeBody tp code =
  Object $
    fromList
      [ ("phoneNumber", Object $ fromList [("id", Number $ fromIntegral $ tpnId tp)])
      , ("securityCode", Object $ fromList [("code", String code)])
      , ("mode", String $ fromMaybe "sms" $ tpnPushMode tp)
      ]
