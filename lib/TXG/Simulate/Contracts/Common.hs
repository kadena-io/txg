{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TXG.Simulate.Contracts.Common
  ( -- * Types
    Account(..)
  , Amount(..)
  , Balance(..)
  , ContractName(..)
  , makeMeta
  , makeMetaWithSender
  , ChainId(..)
  , cidToText
  , cidFromText
  , chainIds
    -- * Accounts
  , accountNames
  , coinAccountNames
  , createPaymentsAccounts
  , createCoinAccount
  , createCoinAccounts
    -- * Generation
  , distinctPair
  , distinctPairSenders
  , distinctPairsSendersOverList
    -- * Parsing
  , parseBytes
    -- * Utils
  , stockKey
    -- * useful constants
  , defGasLimit
  , defGasPrice
  , defTTL
  ) where

import           Control.Lens hiding (elements, uncons, (.=))
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Data.Decimal
import           Data.FileEmbed
import           Data.Function (fix)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Yaml as Y
import           GHC.Generics
import           Pact.ApiReq (ApiKeyPair(..), mkExec, mkKeyPairs)
import qualified Pact.Types.ChainId as CM
import qualified Pact.Types.ChainMeta as CM
import           Pact.Types.Command (Command(..), SomeKeyPairCaps)
import           Pact.Types.Crypto
import           Pact.Types.Gas
import           Text.Printf
import           TXG.Fake
import           TXG.Simulate.Utils
import           TXG.Utils

---

createPaymentsAccount
    :: ChainwebVersion
    -> CM.PublicMeta
    -> String
    -> IO (NEL.NonEmpty SomeKeyPairCaps, Command Text)
createPaymentsAccount v meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- (\k -> [(k, [])]) <$> genKeyPair defaultScheme
    let theData = object
          [ fromText (T.pack (name ++ "-keyset")) .= fmap (formatB16PubKey . fst) nameKeyset
          ]
    res <- mkExec theCode theData meta (NEL.toList adminKS) (Just $ CM.NetworkId $ chainwebVersionToText v) Nothing
    pure (NEL.fromList nameKeyset, res)
  where
    theCode = T.pack $ printf "(payments.create-account \"%s\" %s (read-keyset \"%s-keyset\"))" name (show (1000000.1 :: Decimal)) name

createCoinAccount
    :: ChainwebVersion
    -> CM.PublicMeta
    -> String
    -> IO (NEL.NonEmpty SomeKeyPairCaps, Command Text)
createCoinAccount v meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- NEL.fromList <$> getKeyset
    let theData = object [fromText (T.pack (name ++ "-keyset")) .= fmap (formatB16PubKey . fst) nameKeyset]
    res <- mkExec theCode theData meta (NEL.toList adminKS) (Just $ CM.NetworkId $ chainwebVersionToText v) Nothing
    pure (nameKeyset, res)
  where
    theCode = T.pack $ printf "(coin.create-account \"%s\" (read-keyset \"%s\"))" name name
    isSenderAccount name' = name' `elem` map getAccount coinAccountNames

    getKeyset :: IO [SomeKeyPairCaps]
    getKeyset
      | isSenderAccount name = do
          keypair <- stockKey (T.pack name)
          mkKeyPairs [keypair]
      | otherwise = (\k -> [(k, [])]) <$> genKeyPair defaultScheme

createPaymentsAccounts :: ChainwebVersion -> CM.PublicMeta -> IO (NEL.NonEmpty (NEL.NonEmpty SomeKeyPairCaps, Command Text))
createPaymentsAccounts v meta = traverse (createPaymentsAccount v meta) names

createCoinAccounts :: ChainwebVersion -> CM.PublicMeta -> IO (NEL.NonEmpty (NEL.NonEmpty SomeKeyPairCaps, Command Text))
createCoinAccounts v meta = traverse (createCoinAccount v meta) names

coinAccountNames :: [Account]
coinAccountNames = Account . ("sender0" <>) . show <$> [0 :: Int .. 9]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/devnet/keys.yaml")

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let Right (Object o) = Y.decodeEither' stockKeyFile
      Just (Object kp) = KM.lookup (fromText s) o
      Just (String pub) = KM.lookup "public" kp
      Just (String priv) = KM.lookup "secret" kp
      mkKeyBS = decodeKey . encodeUtf8
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) Nothing

safeCapitalize :: String -> String
safeCapitalize = fromMaybe [] . fmap (uncurry (:) . bimap toUpper (map toLower)) . L.uncons

names :: NEL.NonEmpty String
names = NEL.map safeCapitalize . NEL.fromList $ words "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

newtype Account = Account { getAccount :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype IsString

accountNames :: NEL.NonEmpty Account
accountNames = NEL.map Account names

instance Fake Account where
  fake = elements $ NEL.toList accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    (Amount . realFracToDecimal 12) <$>
    (fromRange @Double (lowerLimit, upperLimit))
    where
      lowerLimit = 0
      upperLimit = 5

newtype Balance = Balance
  { getBalance :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Balance where
  fake = Balance . fromIntegral <$> fromRange (0, 100000 :: Integer)

distinctPair :: (Fake a, Eq a) => FGen (a,a)
distinctPair = fake >>= \a -> (,) a <$> suchThat fake (/= a)

distinctPairSenders :: FGen (Account, Account)
distinctPairSenders = fakeInt 0 9 >>= go
  where
    append num = Account $ "sender0" ++ show num
    go n = do
      m <- fakeInt 0 9
      if n == m then go n else return (append n, append m)

distinctPairsSendersOverList :: [Account] -> FGen (Account, Account)
distinctPairsSendersOverList xs@(_first:_second:_rest) = do
    a <- elements xs
    fix $ \loop -> do
      b <- elements xs
      if a /= b
        then return (a,b)
        else loop
distinctPairsSendersOverList _ = error "distinctPairSendersOverList: Please give at least two accounts!"

-- hardcoded sender (sender00)
makeMeta :: ChainId -> CM.TTLSeconds -> GasPrice -> GasLimit -> IO CM.PublicMeta
makeMeta cid ttl gasPrice gasLimit = do
    t <- currentTxTime
    return $ CM.PublicMeta
        {
          CM._pmChainId = CM.ChainId $ cidToText cid
        , CM._pmSender = "sender00"
        , CM._pmGasLimit = gasLimit
        , CM._pmGasPrice = gasPrice
        , CM._pmTTL = ttl
        , CM._pmCreationTime = t
        }

defGasLimit :: GasLimit
defGasLimit = 600

defGasPrice :: GasPrice
defGasPrice = 0.001

defTTL :: CM.TTLSeconds
defTTL = 3600

makeMetaWithSender :: String -> CM.TTLSeconds -> GasPrice -> GasLimit -> ChainId -> IO CM.PublicMeta
makeMetaWithSender sender ttl gasPrice gasLimit cid =
    set CM.pmSender (T.pack sender) <$> makeMeta cid ttl gasPrice gasLimit

newtype ContractName = ContractName { getContractName :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype Read
  deriving newtype IsString
  deriving newtype ToJSON
  deriving newtype ToJSONKey
  deriving newtype FromJSON
  deriving newtype FromJSONKey

-- instance ToJSON ContractName

-- instance FromJSON ContractName

parseBytes :: MonadThrow m => Text -> Parser a -> B8.ByteString -> m a
parseBytes name parser b = either (throwM . TextFormatException . msg) pure $ parseOnly (parser <* endOfInput) b
  where
    msg e = "Failed to parse " <> T.pack (show b) <> " as " <> name <> ": "
        <> T.pack e

