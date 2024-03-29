{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: TXG.Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module TXG.Utils
(
-- * Chainweb Version
  ChainwebVersion(..)
, chainwebVersionToText
, chainwebVersionFromText

-- * ChainId
, ChainId(..)
, cidToText
, cidFromText
, chainIds

-- * Transaction Time
, currentTxTime

-- * Command Line
, textOption

-- * Misc Utils
, fromJuste
, TextFormatException(..)
, ElasticSearchException(..)
, encodeToText
, unsafeManager

-- * TransactionHash
, TransactionHash(..)
, TransactionHashes(..)

-- * Pact API
, ClientError(..)
, basePath
, pactBasePath
, mempoolBasePath
, pactPost
, mempoolPost
, send
, local
, poll
, listen
, mempoolMember
, spv
) where

import Configuration.Utils

import Control.Monad.Catch

import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX

import GHC.Generics
import GHC.Stack

import Network.Connection
import Network.HostAddress
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import qualified Options.Applicative as O

import qualified Pact.JSON.Encode as J
import Pact.Parse
import Pact.Types.API
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command
import Pact.Types.Hash

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- Chainweb Version

instance Exception TextFormatException

instance Exception ElasticSearchException

data ChainwebVersion
    = Mainnet01
    | Testnet04
    | Development
    | FastDevelopment
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance J.Encode ChainwebVersion where
  build = J.build . toJSON

instance ToJSON ChainwebVersion where
    toJSON = toJSON . chainwebVersionToText
    {-# INLINE toJSON #-}

instance FromJSON ChainwebVersion where
    parseJSON = withText "ChainwebVersion"
        $! either (fail . show) return
        . chainwebVersionFromText
    {-# INLINE parseJSON #-}

chainwebVersionToText :: ChainwebVersion -> T.Text
chainwebVersionToText FastDevelopment = "fast-development"
chainwebVersionToText Development = "development"
chainwebVersionToText Testnet04 = "testnet04"
chainwebVersionToText Mainnet01 = "mainnet01"

chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText "fast-development" = pure FastDevelopment
chainwebVersionFromText "development" = pure Development
chainwebVersionFromText "testnet04" = pure Testnet04
chainwebVersionFromText "mainnet01" = pure Mainnet01
chainwebVersionFromText t = throwM . TextFormatException $ "Unknown Chainweb version: " <> t

-- -------------------------------------------------------------------------- --
-- ChainId

newtype ChainId = ChainId Int
    deriving (Eq, Ord, Num, Generic)
    deriving newtype (ToJSON, FromJSON, Read, Show)

instance J.Encode ChainId where
    build = J.build . toJSON

cidToText :: ChainId -> T.Text
cidToText (ChainId i) = T.pack $ show i

cidFromText :: MonadThrow m => T.Text -> m ChainId
cidFromText t = case readEither (T.unpack t) of
    Left e -> throwM $ TextFormatException $ T.pack e
    Right x -> return x

chainIds :: ChainwebVersion -> [ChainId]
chainIds Mainnet01 = ChainId <$> [0..9]
chainIds Development = ChainId <$> [0..9]
chainIds FastDevelopment = ChainId <$> [0..9]
chainIds Testnet04 = ChainId <$> [0..9]

-- -------------------------------------------------------------------------- --
-- TransactionHash

newtype TransactionHash = TransactionHash B.ByteString
    deriving (Eq, Ord, Generic)

instance Show TransactionHash where
    show = T.unpack . T.decodeUtf8 . BL.toStrict . encode

instance ToJSON TransactionHash where
  toJSON (TransactionHash x) = toJSON $! B64U.encodeBase64Unpadded x

instance FromJSON TransactionHash where
  parseJSON = withText "TransactionHash"
    $! either (fail . T.unpack) return
    . fmap TransactionHash
    . B64U.decodeBase64Unpadded
    . T.encodeUtf8

instance J.Encode TransactionHash where
    build = J.build . toJSON

-- -------------------------------------------------------------------------- --
-- Time

currentTxTime :: IO CM.TxCreationTime
currentTxTime = CM.TxCreationTime . ParsedInteger . round <$> getPOSIXTime


-- -------------------------------------------------------------------------- --
-- Misc Utils

fromJuste :: HasCallStack => Maybe a -> a
fromJuste = fromJust

newtype TextFormatException = TextFormatException T.Text
    deriving (Show, Eq, Ord, IsString, Generic)

encodeToText :: ToJSON a => a -> T.Text
encodeToText = T.decodeUtf8 . BL.toStrict . encode

unsafeManager :: IO Manager
unsafeManager = newTlsManagerWith
    $ mkManagerSettings (TLSSettingsSimple True True True) Nothing

newtype ElasticSearchException = ElasticSearchException B.ByteString
    deriving (Show, Eq, Ord, IsString, Generic)

-- -------------------------------------------------------------------------- --
-- Commad line

textOption :: (T.Text -> Either SomeException a) -> Mod OptionFields a -> O.Parser a
textOption f = option $ eitherReader $ first show . f . T.pack

-- -------------------------------------------------------------------------- --
-- Pact API

newtype ClientError = ClientError T.Text
    deriving (Show, Eq, Ord, Generic)

instance Exception ClientError

post
    :: J.Encode body
    => FromJSON result
    => Manager
    -> HostAddress
    -> T.Text
    -> Bool
    -> body
    -> IO (Either ClientError result)
post mgr hostAddr urlPath s body = do
    resp <- httpLbs req mgr
    return $ if statusIsSuccessful (responseStatus resp)
        then first (ClientError . T.pack)
            $ eitherDecode'
            $ responseBody resp
        else Left
            $ ClientError
            $ encodeToText
            $ object
                [ "status" .= show (responseStatus resp)
                , "body" .= T.decodeUtf8 (BL.toStrict $ responseBody resp)
                ]
  where
    req = defaultRequest
        { method = "POST"
        , secure = s
        , host = T.encodeUtf8 $ hostnameToText $ _hostAddressHost hostAddr
        , port = fromIntegral $ _hostAddressPort hostAddr
        , path = T.encodeUtf8 urlPath
        , requestHeaders = [("content-type", "application/json")]
        , requestBody = RequestBodyLBS $ J.encode body
        , responseTimeout = responseTimeoutMicro $ 1000000 * 60 * 4
        }

pactPost
    :: J.Encode body
    => FromJSON result
    => Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> T.Text
    -> body
    -> IO (Either ClientError result)
pactPost mgr hostAddr v cid pactPath
    = post mgr hostAddr (pactBasePath v cid <> pactPath) False

mempoolPost
    :: J.Encode body
    => FromJSON result
    => Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> T.Text
    -> body
    -> IO (Either ClientError result)
mempoolPost mgr hostAddr v cid mempoolPath
    = post mgr hostAddr (mempoolBasePath v cid <> mempoolPath) False

chainBasePath :: ChainwebVersion -> ChainId -> T.Text
chainBasePath v cid = basePath v <> "/chain/" <> cidToText cid

basePath :: ChainwebVersion -> T.Text
basePath v = "chainweb/0.0/" <> chainwebVersionToText v

pactBasePath :: ChainwebVersion -> ChainId -> T.Text
pactBasePath v cid = chainBasePath v cid <> "/pact/api/v1"

mempoolBasePath :: ChainwebVersion -> ChainId -> T.Text
mempoolBasePath v cid = chainBasePath v cid <> "/mempool"

send
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> [Command T.Text]
    -> IO (Either ClientError RequestKeys)
send m a v c xs = pactPost m a v c "/send" (SubmitBatch (NEL.fromList xs))

poll
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
poll m a v c rkeys = pactPost m a v c "/poll" $ Poll (_rkRequestKeys rkeys)

local
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> Command T.Text
    -> IO (Either ClientError (CommandResult Hash))
local m a v c cmdText = pactPost m a v c "/local" cmdText

listen
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> RequestKey
    -> IO (Either ClientError ListenResponse)
listen m a v c rk = pactPost m a v c "/listen" (ListenerRequest rk)

newtype TransactionHashes = TransactionHashes [TransactionHash]
    deriving (Show, Eq, Ord, Generic)

instance J.Encode TransactionHashes where
    build (TransactionHashes xs) = J.build $ J.Array xs

mempoolMember
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> TransactionHashes
    -> IO (Either ClientError [Bool])
mempoolMember m a v c txh = mempoolPost m a v c "/member" txh

spv
    :: Manager
    -> HostAddress
    -> ChainwebVersion
    -> ChainId
    -> RequestKey
    -> IO (Either ClientError T.Text)
spv m a v c rk = pactPost m a v c "/spv" $ J.object
  [
    "requestKey" J..= rk
  , "targetChainId" J..= c
  ]
