{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications           #-}

-- | Module: TXG.Types
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TXG.Types
  ( -- * TXCmd
    TXCmd(..)
  , transactionCommandBytes
  , transactionCommandToText
  , transactionCommandFromText
  , SingleTX(..)
    -- * Timing
  , TimingDistribution(..)
  , Gaussian(..), Uniform(..)
  , defaultTimingDist
    -- * Args
  , Args(..)
  , defaultArgs
  , scriptConfigParser
    -- * TXG Monad
  , TXG(..)
  , TXGState(..)
  , TXGConfig(..), mkTXGConfig
    -- * Pact API
  , pactPost
  , ClientError(..)
  , unsafeManager
    -- * Misc.
  , ChainwebHost(..)
  , TXCount(..)
  , BatchSize(..)
  , ElasticSearchConfig(..)
  , PollMap
  , PollParams(..)
  , Verbose(..)
  , nelReplicate
  , nelZipWith3
  , toNodeData
  , httpJson
  ) where

import           Configuration.Utils hiding (Error, Lens')
import           Control.Concurrent.STM
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Primitive
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Decimal
import           Data.Function
import           Data.Generics.Product.Fields (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import           Data.Sequence.NonEmpty (NESeq(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Int
import           GHC.Generics
import           Network.HostAddress
import           Network.HTTP.Client hiding (Proxy)
import qualified Options.Applicative as O
import qualified Pact.JSON.Encode as J
import           Pact.Parse
import           Pact.Types.API
import           Pact.Types.Capability (SigCapability)
import           Pact.Types.ChainMeta
import           Pact.Types.Command (DynKeyPair)
import           Pact.Types.Gas
import           System.Random.MWC (Gen)
import           System.Logger
import           Text.Read (readEither)
import           Text.Printf
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Utils

import           GHC.Stack

newtype Verbose = Verbose { verbosity :: Bool }
  deriving (Eq, Show, Generic, Read)
  deriving anyclass (FromJSON, ToJSON)

data TimingDistribution = GaussianTD Gaussian | UniformTD Uniform
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Gaussian = Gaussian { mean :: !Double, var :: !Double }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Uniform = Uniform { low :: !Double, high :: !Double }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultTimingDist :: TimingDistribution
defaultTimingDist = GaussianTD $ Gaussian 1000000 (1000000 / 16)

data TXCmd
  = DeployContracts [Sim.ContractName]
  | RunStandardContracts TimingDistribution
  | RunCoinContract TimingDistribution
  | RunXChainTransfer TimingDistribution
  | RunSimpleExpressions TimingDistribution
  | PollRequestKeys Text
  | ListenerRequestKey Text
  | SingleTransaction SingleTX
  | MempoolMember (ChainId, [TransactionHash])
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SingleTX = SingleTX { _stxCmd :: Text, _stxChainId :: ChainId }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

transactionCommandToText :: TXCmd -> Text
transactionCommandToText = T.decodeUtf8 . fromJuste . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TXCmd -> Maybe B8.ByteString
transactionCommandBytes t = case t of
  PollRequestKeys bs    -> Just $ T.encodeUtf8 $ "poll [" <> bs <> "]"
  ListenerRequestKey bs -> Just $ T.encodeUtf8 $ "listen " <> bs
  _                     -> Nothing

transactionCommandFromText :: MonadThrow m => Text -> m TXCmd
transactionCommandFromText = readTXCmdBytes . T.encodeUtf8
{-# INLINE transactionCommandFromText #-}

readTXCmdBytes :: MonadThrow m => B8.ByteString -> m TXCmd
readTXCmdBytes = Sim.parseBytes "transaction-command" transactionCommandParser
{-# INLINE readTXCmdBytes #-}

transactionCommandParser :: A.Parser TXCmd
transactionCommandParser
    = pollkeys
    <|> listenkeys
    <|> (RunCoinContract defaultTimingDist <$ "transfers")
    <|> (RunSimpleExpressions defaultTimingDist <$ "simple")

pollkeys :: A.Parser TXCmd
pollkeys = PollRequestKeys . T.decodeUtf8
    <$> ("poll" *> s *> "[" *> s *> parseRequestKey <* s <* "]")
  where
    s = A.skipSpace

-- This is brittle!
parseRequestKey :: A.Parser ByteString
parseRequestKey = B8.pack <$> A.count 43 A.anyChar

listenkeys :: A.Parser TXCmd
listenkeys = ListenerRequestKey . T.decodeUtf8
    <$> ("listen" *> s *> parseRequestKey)
  where
    s = A.skipSpace

-------
-- Args
-------


data ChainwebHost = ChainwebHost
  {
    cwh_hostname :: Hostname
  , cwh_p2pPort :: Port
  , cwh_servicePort :: Port
  } deriving (Show, Generic)

instance ToJSON ChainwebHost where
  toJSON o = object
    [
      "hostname" .= cwh_hostname o
    , "p2pPort" .= cwh_p2pPort o
    , "servicePort" .= cwh_servicePort o
    ]

instance FromJSON ChainwebHost where
  parseJSON = withObject "ChainwebHost" $ \o -> ChainwebHost
    <$> o .: "hostname"
    <*> o .: "p2pPort"
    <*> o .: "servicePort"

data ElasticSearchConfig = ElasticSearchConfig
  { esHost :: !Hostname
  , esPort :: !Port
  , esIndex :: !Text
  , esApiKey :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON ElasticSearchConfig where
  toJSON o = object
    [ "esHost" .= esHost o
    , "esPort" .= esPort o
    , "esIndex" .= esIndex o
    , "esApiKey" .= esApiKey o
    ]

instance FromJSON ElasticSearchConfig where
  parseJSON = withObject "ElasticSearchConfig" $ \o -> ElasticSearchConfig
    <$> o .: "esHost"
    <*> o .: "esPort"
    <*> o .: "esIndex"
    <*> o .: "esApiKey"

data Args = Args
  { scriptCommand   :: !TXCmd
  , nodeChainIds    :: ![ChainId]
  , isChainweb      :: !Bool
  , chainwebHosts   :: ![ChainwebHost]
  , nodeVersion     :: !ChainwebVersion
  , batchSize       :: !BatchSize
  , verbose         :: !Verbose
  , gasLimit        :: GasLimit
  , gasPrice        :: GasPrice
  , timetolive      :: TTLSeconds
  , trackMempoolStatConfig :: !(Maybe PollParams)
  , confirmationDepth :: !Int
  , logLevel :: !LogLevel
  , elasticSearchConfig :: !(Maybe ElasticSearchConfig)
  } deriving (Show, Generic)

instance J.Encode Args where
  build = J.build . toJSON

instance ToJSON Args where
  toJSON o = object
    [ "scriptCommand"   .= scriptCommand o
    , "nodeChainIds"    .= nodeChainIds o
    , "isChainweb"      .= isChainweb o
    , "chainwebHosts"   .= chainwebHosts o
    , "chainwebVersion" .= nodeVersion o
    , "batchSize"       .= batchSize o
    , "verbose"         .= verbose o
    , "gasLimit"        .= toJsonViaEncode @GasLimit (gasLimit o)
    , "gasPrice"        .= toJsonViaEncode @GasPrice (gasPrice o)
    , "timetolive"      .= toJsonViaEncode @TTLSeconds (timetolive o)
    , "trackMempoolStatConfig" .= trackMempoolStatConfig o
    , "confirmationDepth" .= confirmationDepth o
    , "logLevel" .= logLevel o
    , "elasticSearchConfig" .= elasticSearchConfig o
    ]

toJsonViaEncode :: HasCallStack => J.Encode a => a -> Value
toJsonViaEncode a = case eitherDecode (J.encode a) of
  Left e -> error $ "Pact.JSON.Encode.toJsonViaEncode: value does not roundtrip. This is a bug in pact-json" <> e
  Right r -> r

instance FromJSON (Args -> Args) where
  parseJSON = withObject "Args" $ \o -> id
    <$< field @"scriptCommand"   ..: "scriptCommand"   % o
    <*< field @"nodeChainIds"    ..: "nodeChainIds"    % o
    <*< field @"isChainweb"      ..: "isChainweb"      % o
    <*< field @"chainwebHosts"   ..: "chainwebHosts"   % o
    <*< field @"nodeVersion"     ..: "chainwebVersion" % o
    <*< field @"batchSize"       ..: "batchSize"       % o
    <*< field @"verbose"         ..: "verbose"         % o
    <*< field @"gasLimit"        ..: "gasLimit"        % o
    <*< field @"gasPrice"        ..: "gasPrice"        % o
    <*< field @"timetolive"      ..: "timetolive"      % o
    <*< field @"trackMempoolStatConfig" ..: "trackMempoolStatConfig" % o
    <*< field @"confirmationDepth" ..: "confirmationDepth" % o
    <*< field @"logLevel" ..: "logLevel" % o
    <*< field @"elasticSearchConfig" ..: "elasticSearchConfig" % o

defaultArgs :: Args
defaultArgs = Args
  { scriptCommand   = RunSimpleExpressions defaultTimingDist
  , nodeChainIds    = []
  , isChainweb      = True
  , chainwebHosts   = []
  , nodeVersion     = v
  , batchSize       = BatchSize 1
  , verbose         = Verbose False
  , gasLimit = Sim.defGasLimit
  , gasPrice = Sim.defGasPrice
  , timetolive = Sim.defTTL
  , trackMempoolStatConfig = Just defaultPollParams
  , confirmationDepth = 6
  , logLevel = Info
  , elasticSearchConfig = Nothing
  }
  where
    v :: ChainwebVersion
    v = Development

defaultPollParams :: PollParams
defaultPollParams = PollParams
  {
    pollChunkSize = 1
  , pollExponentialBackoffInitTime = 50000
  , pollRetries = 10
  }

scriptConfigParser :: MParser Args
scriptConfigParser = id
  <$< field @"scriptCommand" .:: textOption transactionCommandFromText
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll', 'listen', 'transfers', and 'simple'.")
  <*< field @"nodeChainIds" %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< field @"chainwebHosts" %:: pLeftSemigroupalUpdate (pure <$> pChainwebHost)
  <*< field @"nodeVersion" .:: textOption chainwebVersionFromText
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  <*< field @"batchSize" .:: option auto
      % long "batch-size"
      <> short 'b'
      <> metavar "COUNT"
      <> help "Number of transactions to bundle into a single 'send' call"
  <*< field @"verbose" .:: option auto
      % long "verbose"
      <> metavar "BOOL"
      <> help "Whether to print out details of each transaction in a 'send' call"
  <*< field @"gasLimit" .:: option parseGasLimit
      % long "gasLimit"
      <> metavar "INT"
      <> help "The gas limit of each auto-generated transaction"
  <*< field @"gasPrice" .:: option parseGasPrice
      % long "gasPrice"
      <> metavar "DECIMAL"
      <> help "The gas price of each auto-generated transaction"
  <*< field @"timetolive" .:: option parseTTL
      % long "time-to-live"
      <> metavar "SECONDS"
      <> help "The time to live (in seconds) of each auto-generated transaction"
  <*< field @"trackMempoolStatConfig" .:: option auto
      % long "track-mempool-stat-config"
      <> metavar "CONFIG"
      <> help "Wether to print out mempool related statistics"
  <*< field @"confirmationDepth" .:: option auto
      % long "confirmation-depth"
      <> metavar "INT"
      <> help "Confirmation depth"
  <*< field @"logLevel" .:: pLogLevel
  <*< field @"elasticSearchConfig" .:: pElasticSearchConfig
  where
    read' :: Read a => String -> ReadM a
    read' msg = eitherReader (bimap (const msg) id . readEither)
    parseGasLimit =
        GasLimit . ParsedInteger <$> read' @Integer "Cannot read gasLimit."
    parseGasPrice =
        GasPrice . ParsedDecimal <$> read' @Decimal "Cannot read gasPrice."
    parseTTL =
        TTLSeconds . ParsedInteger <$> read' @Integer "Cannot read time-to-live."

pChainwebHost :: O.Parser ChainwebHost
pChainwebHost = ChainwebHost
  <$> pHostname Nothing
  <*> pPort (Just "p2p")
  <*> pPort (Just "service")

pChainId :: O.Parser ChainId
pChainId = textOption cidFromText
  % long "node-chain-id"
  <> short 'i'
  <> metavar "INT"
  <> help "The specific chain that will receive generated transactions. Can be used multiple times."

pElasticSearchConfig :: O.Parser (Maybe ElasticSearchConfig)
pElasticSearchConfig = optional $ ElasticSearchConfig
  <$> pHostname (Just "elastic-search")
  <*> pPort (Just "elastic-search")
  <*> pIndexName (Just "transaction-generator")
  <*> pApiKey Nothing


pApiKey :: Maybe Text -> O.Parser (Maybe Text)
pApiKey def = optional $ strOption
  % long "elastic-search-api-key"
  <> short 'k'
  <> metavar "STRING"
  <> help "The api key to use for elastic search."
  <> value (foldr const "" def)

pIndexName :: Maybe Text -> O.Parser Text
pIndexName def = strOption
  % long "elastic-search-index-name"
  <> short 'n'
  <> metavar "STRING"
  <> help "The name of the index to write to."
  <> value (foldr const "transaction-generator" def)

------------
-- TXG Monad
------------

-- TODO: Ideally we'd shove `LoggerT` into this stack, but `yet-another-logger`
-- would have to be patched to add missing instances first. Having `LoggerT`
-- here would let us remove the `MonadTrans` instance, as well as a number of
-- `lift` calls.

-- | The principal application Monad for this Transaction Generator.
newtype TXG s m a = TXG { runTXG :: ReaderT TXGConfig (StateT s m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState s, MonadReader TXGConfig)

instance MonadTrans (TXG s) where
  lift = TXG . lift . lift

data TXGState = TXGState
  { gsGen     :: !(Gen (PrimState IO))
  , gsCounter :: !(TVar TXCount)
  , gsChains  :: !(NESeq ChainId)
  } deriving (Generic)

data TXGConfig = TXGConfig
  { confTimingDist :: !(Maybe TimingDistribution)
  , confKeysets :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))))
  , confHost :: !HostAddress
  , confManager :: !Manager
  , confVersion :: !ChainwebVersion
  , confBatchSize :: !BatchSize
  , confVerbose :: !Verbose
  , confGasLimit :: GasLimit
  , confGasPrice :: GasPrice
  , confTTL :: TTLSeconds
  , confTrackMempoolStat :: !(Maybe PollParams)
  , confElasticSearchConfig :: !(Maybe ElasticSearchConfig)
  } deriving (Generic)

mkTXGConfig :: Maybe TimingDistribution -> Args -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config hostAddr = do
  mgr <- unsafeManager
  pure $ TXGConfig
    { confTimingDist = mdistribution
    , confKeysets = mempty
    , confHost = hostAddr
    , confManager = mgr
    , confVersion = nodeVersion config
    , confBatchSize = batchSize config
    , confVerbose = verbose config
    , confGasLimit = gasLimit config
    , confGasPrice = gasPrice config
    , confTTL = timetolive config
    , confTrackMempoolStat = trackMempoolStatConfig config
    , confElasticSearchConfig = elasticSearchConfig config
    }

-- -------------------------------------------------------------------------- --
-- MISC

-- | A running count of all transactions handles over all threads.
newtype TXCount = TXCount Word
  deriving newtype (Num, Show)

newtype BatchSize = BatchSize Word
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq, Read, Show, ToJSON, FromJSON)

data PollParams = PollParams
  {
    pollChunkSize :: !Int
  , pollExponentialBackoffInitTime :: !Int
  , pollRetries :: !Int
  }
  deriving (Show, Generic, Read)

instance ToJSON PollParams where
  toJSON o = object
    [
      "numChunks" .= pollChunkSize o
    , "exponentialBackoff" .= pollExponentialBackoffInitTime o
    , "retries" .= pollRetries o
    ]

instance FromJSON PollParams where
  parseJSON = withObject "PollParams" $ \o -> PollParams
      <$> o .: "numChunks"
      <*> o .: "exponentialBackoff"
      <*> o .: "retries"

nelReplicate :: Word -> a -> NEL.NonEmpty a
nelReplicate n a = NEL.unfoldr f n
  where
    f 0 = error "nelReplicate: Can't have length-0 list."
    f 1 = (a, Nothing)
    f m = (a, Just $ m - 1)

nelZipWith3 :: (a -> b -> c -> d) -> NEL.NonEmpty a -> NEL.NonEmpty b -> NEL.NonEmpty c -> NEL.NonEmpty d
nelZipWith3 f ~(x NEL.:| xs) ~(y NEL.:| ys) ~(z NEL.:| zs) = f x y z NEL.:| zipWith3 f xs ys zs

type PollMap = Map NodeData (Map ChainId [(TimeSpan, RequestKeys)])

data NodeData = NodeData
  {
    nodeData_key :: !Int
  , nodeData_name :: !T.Text
  } deriving Show

instance Eq NodeData where
  (==) = on (==) nodeData_key

instance Ord NodeData where
  compare = on compare nodeData_key

toNodeData :: Int -> ChainwebHost -> NodeData
toNodeData nodeKey ch = NodeData
  {
    nodeData_key = nodeKey
  , nodeData_name = T.pack $ printf "%s:%s" (show $ cwh_hostname ch) (show $ cwh_servicePort ch)
  }

data TimeSpan = TimeSpan
  {
    start_time :: Int64
  , end_time :: Int64
  } deriving Show

httpJson :: FromJSON a => Manager -> Request -> IO a
httpJson mgr req = do
    resp <- httpLbs req mgr
    case eitherDecode (responseBody resp) of
        Left e -> error e
        Right a -> return a

