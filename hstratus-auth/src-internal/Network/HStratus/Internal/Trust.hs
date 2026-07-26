{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Module      : Network.HStratus.Internal.Trust
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD-3-Clause

Internal types and functions for two-factor authentication trust management.
-}
module Network.HStratus.Internal.Trust
  ( -- * data types
    CodeStatus (..)
  , TrustedPhone (..)
  , TrustedDevice (..)
  , TrustedList (..)
  , TrustData (..)
  , Setup2SADevice (..)

    -- * functions
  , withSelectedPhoneOrDevice
  , pleaseReadCode
  , pleaseChooseN
  , selectPhone
  , selectDevice
  , selectTwoFaPhone
  , setup2SADeviceLabel
  , selectSetupDevice
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch, throwIO)
import Data.Aeson
  ( FromJSON (..)
  , KeyValue (..)
  , Object
  , Options (..)
  , SumEncoding (ObjectWithSingleField)
  , ToJSON (..)
  , Value (..)
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  , object
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.KeyMap (filterWithKey)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import SimplePrompt (promptNonEmpty)
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)


putDeviceChoice :: (Int, TrustedDevice) -> IO ()
putDeviceChoice (i, td)
  | tdModelName td == "" =
      Text.putStrLn $ Text.pack (show i) <> ") " <> tdName td <> "\tSMS\t" <> tdId td
  | otherwise =
      Text.putStrLn $ Text.pack (show i) <> ") " <> tdName td <> "\t" <> tdModelName td <> "\t" <> tdId td


-- idx is 1-based and in [1, length xs], as enforced by pleaseChooseN
nthOf :: NonEmpty a -> Int -> a
nthOf xs idx = toList xs !! (idx - 1)


-- | Prompt the user to choose one device from a non-empty list of trusted devices.
selectDevice :: NonEmpty TrustedDevice -> IO TrustedDevice
selectDevice xs = do
  Text.putStrLn "Please select a trusted device to send a code to"
  mapM_ putDeviceChoice $ zip ([1 ..] :: [Int]) (toList xs)
  idx <- pleaseChooseN 1 (length xs)
  pure (nthOf xs idx)


-- | Prompt the user to choose one phone number from a non-empty list of trusted phones.
selectPhone :: NonEmpty TrustedPhone -> IO TrustedPhone
selectPhone xs = do
  let putPhoneChoice (i, x) = Text.putStrLn $ Text.pack (show i) <> ") " <> tpnNumberWithDialCode x
  Text.putStrLn "Please select a trusted phone number to send a code to"
  mapM_ putPhoneChoice $ zip ([1 ..] :: [Int]) (toList xs)
  idx <- pleaseChooseN 1 (length xs)
  pure (nthOf xs idx)


-- | Prompt the user to enter an integer in the inclusive range @[low, high]@, retrying on invalid input.
pleaseChooseN :: Int -> Int -> IO Int
pleaseChooseN low high = do
  let prefix = "Please choose an option between " <> show low <> " and " <> show high
  result <- (readMaybe <$> promptNonEmpty prefix) `catch` onEof
  case result of
    Nothing -> pleaseChooseN low high
    Just x | x < low || x > high -> pleaseChooseN low high
    Just x -> pure x
 where
  onEof :: IOException -> IO (Maybe Int)
  onEof e
    | isEOFError e = throwIO (userError "unexpected end of input")
    | otherwise = throwIO e


-- | Prompt the user to enter a security code of the given length.
pleaseReadCode :: Word8 -> IO Text
pleaseReadCode len = do
  let prefix = "Please enter the " <> show len <> "-digit code you just received"
  Text.pack <$> promptNonEmpty prefix


-- | Information describing the status of the security code verification
data CodeStatus = CodeStatus
  { scLength :: !Word8
  -- ^ expected number of digits in the security code
  , scTooManyCodesSent :: !Bool
  -- ^ @True@ when Apple has refused to send further codes
  , scTooManyCodesValidated :: !Bool
  -- ^ @True@ when the verification attempt limit has been reached
  , scSecurityCodeLocked :: !Bool
  -- ^ @True@ when the security code gate is locked
  , scSecurityCodeCooldown :: !Bool
  -- ^ @True@ when a cooldown period is active before a new code can be sent
  }
  deriving (Eq, Show, Generic)


instance FromJSON CodeStatus where
  parseJSON = withObject "CodeStatus" $ \o ->
    CodeStatus
      <$> o .: "length"
      <*> (fromMaybe False <$> o .:? "tooManyCodesSent")
      <*> (fromMaybe False <$> o .:? "tooManyCodesValidated")
      <*> (fromMaybe False <$> o .:? "securityCodeLocked")
      <*> (fromMaybe False <$> o .:? "securityCodeCooldown")


instance ToJSON CodeStatus where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | A trusted phone number registered for two-factor verification
data TrustedPhone = TrustedPhone
  { tpnId :: !Word8
  -- ^ Apple's internal identifier for this phone number
  , tpnNumberWithDialCode :: !Text
  -- ^ display string including the country dial code, e.g. @"+1 (•••) •••-1234"@
  , tpnPushMode :: !(Maybe Text)
  -- ^ push delivery mode (e.g. @"sms"@); @Nothing@ when absent
  }
  deriving (Eq, Show, Generic)


instance FromJSON TrustedPhone where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON TrustedPhone where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | Information about a trusted device
data TrustedDevice = TrustedDevice
  { tdId :: !Text
  -- ^ Apple's internal identifier for this device
  , tdName :: !Text
  -- ^ human-readable device name, e.g. @"Tim's iPhone"@
  , tdModelName :: !Text
  -- ^ model string, e.g. @"iPhone 15 Pro"@; empty string when absent
  }
  deriving (Eq, Show, Generic)


instance FromJSON TrustedDevice where
  parseJSON = withObject "TrustedDevice" $ \o ->
    TrustedDevice
      <$> o .: "id"
      <*> o .: "name"
      <*> (fromMaybe "" <$> o .:? "modelName")


instance ToJSON TrustedDevice where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | A non-empty list of @TrustedPhone@ or @TrustedDevice@
data TrustedList
  = -- | the account has trusted phone numbers but no trusted devices
    TrustedPhoneNumbers !(NonEmpty TrustedPhone)
  | -- | the account has trusted devices (and may also have trusted phone numbers)
    TrustedDevices !(NonEmpty TrustedDevice)
  deriving (Eq, Show, Generic)


instance FromJSON TrustedList where
  parseJSON = genericParseJSON trustedListOptions


instance ToJSON TrustedList where
  toJSON = genericToJSON trustedListOptions
  toEncoding = genericToEncoding trustedListOptions


trustedListOptions :: Options
trustedListOptions =
  ( simpleOptions
      { sumEncoding = ObjectWithSingleField
      , constructorTagModifier = camelCase
      }
  )


data TrustData = TrustData
  { tdList :: !TrustedList
  -- ^ trusted phones or devices that can receive a verification code
  , tdSecurityCode :: !CodeStatus
  -- ^ current status of the security-code gate (length, lockout flags)
  , tdNoTrustedDevices :: !Bool
  -- ^ @True@ when no trusted devices are registered; only phone numbers available
  }
  deriving (Eq, Show)


-- | Selects a phone/device and applies the appropriate handler
withSelectedPhoneOrDevice
  :: (TrustedPhone -> IO a) -> (TrustedDevice -> IO a) -> TrustData -> IO a
withSelectedPhoneOrDevice handlePhone handleDevice = do
  let ikou (TrustedDevices ys) = selectDevice ys >>= handleDevice
      ikou (TrustedPhoneNumbers (y :| [])) = handlePhone y
      ikou (TrustedPhoneNumbers ys) = selectPhone ys >>= handlePhone
  ikou . tdList


toJSONTrustData :: TrustData -> Value
toJSONTrustData td =
  let asPairs (Object o) = KeyMap.toList o
      asPairs _other = []
      fromOthers =
        [ "securityCode" .= tdSecurityCode td
        , "noTrustedDevices" .= tdNoTrustedDevices td
        ]
      fromTrustedList = asPairs $ toJSON $ tdList td
   in object $ fromOthers <> fromTrustedList


parseJSONTrustData :: Value -> Parser TrustData
parseJSONTrustData = withObject "TrustData" $ \o ->
  let securityCode = o .: "securityCode"
      noTrustedDevices = fromMaybe False <$> o .:? "noTrustedDevices"
      isListKey key _ignored = key == "trustedPhoneNumbers" || key == "trustedDevices"
      theList = parseJSON (Object $ filterWithKey isListKey o)
   in TrustData <$> theList <*> securityCode <*> noTrustedDevices


instance ToJSON TrustData where
  toJSON = toJSONTrustData


instance FromJSON TrustData where
  parseJSON = parseJSONTrustData


-- | An opaque device record used in the legacy 2SA flow; fields are Apple-defined JSON.
newtype Setup2SADevice = Setup2SADevice {setup2SAFields :: Object}
  deriving (Eq, Show)


instance FromJSON Setup2SADevice where
  parseJSON = withObject "Setup2SADevice" (pure . Setup2SADevice)


instance ToJSON Setup2SADevice where
  toJSON (Setup2SADevice o) = Object o


-- | Extract a human-readable label from a 2SA setup device, falling back to @"(unknown)"@.
setup2SADeviceLabel :: Setup2SADevice -> Text
setup2SADeviceLabel (Setup2SADevice o) = fromMaybe "(unknown)" $ do
  v <- lookup "phoneNumber" pairs <|> lookup "name" pairs
  case v of
    String t -> Just t
    _ -> Nothing
 where
  pairs = KeyMap.toList o


-- | Select a trusted phone from 'TrustData' for 2FA, prompting the user when multiple phones are available.  Returns 'Nothing' when the user opts for a trusted device instead.
selectTwoFaPhone :: TrustData -> IO (Maybe TrustedPhone)
selectTwoFaPhone td =
  let phones = case tdList td of
        TrustedPhoneNumbers ps -> toList ps
        TrustedDevices _ -> []
   in if tdNoTrustedDevices td
        then pure (listToMaybe phones)
        else pickPhoneOrDevice phones
 where
  pickPhoneOrDevice [] = pure Nothing
  pickPhoneOrDevice phones = do
    mapM_
      (\(i, p) -> Text.putStrLn $ Text.pack (show (i :: Int)) <> ") " <> tpnNumberWithDialCode p)
      (zip [1 ..] phones)
    Text.putStrLn "Press Enter to use a trusted device, or select a phone number by its index to receive an SMS:"
    response <- Text.getLine
    if Text.null response
      then pure Nothing
      else case readMaybe (Text.unpack response) of
        Just n | n >= (1 :: Int) && n <= length phones -> pure $ listToMaybe $ drop (n - 1) phones
        _ -> pickPhoneOrDevice phones


-- | Prompt the user to choose a trusted device to receive a legacy 2SA verification code.
selectSetupDevice :: NonEmpty Setup2SADevice -> IO Setup2SADevice
selectSetupDevice xs = do
  Text.putStrLn "Please select a trusted device to receive a verification code"
  mapM_ (\(i, d) -> Text.putStrLn $ Text.pack (show (i :: Int)) <> ") " <> setup2SADeviceLabel d) (zip [1 ..] (toList xs))
  idx <- pleaseChooseN 1 (length xs)
  pure (nthOf xs idx)


simpleOptions :: Options
simpleOptions = aesonPrefix camelCase
