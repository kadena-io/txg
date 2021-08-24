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
    -- * Args
  , Args(..)
  , defaultArgs
  , scriptConfigParser
    -- * TXG Monad
  , TXG(..)
  , TXGState(..)
  , TXGConfig(..), mkTXGConfig
  -- * MPT Args
  , MPTArgs(..)
  , defaultMPTArgs
  , mpt_scriptConfigParser
    -- * Pact API
  , pactPost
  , ClientError(..)
  , unsafeManager
    -- * Misc.
  , TXCount(..)
  , BatchSize(..)
  , Verbose(..)
  , nelReplicate
  , nelZipWith3
  ) where

import           Configuration.Utils hiding (Error, Lens')
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Primitive
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Decimal
import           Data.Generics.Product.Fields (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import           Data.Sequence.NonEmpty (NESeq(..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           GHC.Generics
import           Network.HostAddress
import           Network.HTTP.Client hiding (Proxy)
import qualified Options.Applicative as O
import           Pact.Parse
import           Pact.Types.ChainMeta
import           Pact.Types.Command (SomeKeyPairCaps)
import           Pact.Types.Gas
import           System.Random.MWC (Gen)
import           Text.Read (readEither)
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Utils

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


data MPTArgs = MPTArgs
  {
    mpt_transferRate      :: !Int -- per second
  , mpt_senderAccount     :: !Text
  , mpt_rcvAccount        :: !Text
  , mpt_batchSize         :: !BatchSize
  , mpt_confirmationDepth :: !Int
  , mpt_verbose           :: !Verbose
  , mpt_gasLimit          :: GasLimit
  , mpt_gasPrice          :: GasPrice
  , mpt_timetolive        :: TTLSeconds
  , mpt_hostAddresses   :: ![HostAddress]
  , mpt_nodeVersion     :: !ChainwebVersion
  } deriving (Show, Generic)


instance ToJSON MPTArgs where
  toJSON o = object
    [
      "transferRate"      .= mpt_transferRate o
    , "senderAccount"     .= mpt_senderAccount o
    , "rcvAccount"        .= mpt_rcvAccount o
    , "batchSize"         .= mpt_batchSize o
    , "confirmationDepth" .= mpt_confirmationDepth o
    , "verbose"           .= mpt_verbose o
    , "gasLimit"          .= mpt_gasLimit o
    , "gasPrice"          .= mpt_gasPrice o
    , "timetolive"        .= mpt_timetolive o
    , "hostAddresses"     .= mpt_hostAddresses o
    , "nodeVersion"       .= mpt_nodeVersion o
    ]

instance FromJSON (MPTArgs -> MPTArgs) where
  parseJSON = withObject "MPTArgs" $ \o -> id
    <$< field @"mpt_transferRate" ..: "mpt_transferRate" % o
    <*< field @"mpt_senderAccount" ..: "mpt_senderAccount" % o
    <*< field @"mpt_rcvAccount" ..: "mpt_rcvAccount" % o
    <*< field @"mpt_batchSize" ..: "mpt_batchSize" % o
    <*< field @"mpt_confirmationDepth" ..: "mpt_confirmationDepth" % o
    <*< field @"mpt_verbose" ..: "mpt_verbose" % o
    <*< field @"mpt_gasLimit" ..: "mpt_gasLimit" % o
    <*< field @"mpt_gasPrice" ..: "mpt_gasPrice" % o
    <*< field @"mpt_timetolive" ..: "mpt_timetolive" % o
    <*< field @"mpt_hostAddresses" ..: "mpt_hostAddresses" % o
    <*< field @"mpt_nodeVersion" ..: "mpt_nodeVersion" % o

defaultMPTArgs :: MPTArgs
defaultMPTArgs = MPTArgs
  {
    mpt_transferRate      = 5
  , mpt_senderAccount     = "sender01"
  , mpt_rcvAccount        = "sender02"
  , mpt_batchSize         = BatchSize 1
  , mpt_confirmationDepth = 6
  , mpt_verbose           = Verbose False
  , mpt_gasLimit          = Sim.defGasLimit
  , mpt_gasPrice          = Sim.defGasPrice
  , mpt_timetolive        = Sim.defTTL
  , mpt_hostAddresses     = mempty
  , mpt_nodeVersion       = Development
  }

mpt_scriptConfigParser :: MParser MPTArgs
mpt_scriptConfigParser = id
  <$< field @"mpt_transferRate" .:: option auto
    % long "transfer-rate"
    <> metavar "INT"
    <> help "How many transaction batches should be sent per request"
  <*< field @"mpt_senderAccount" .:: option auto
    % long "sender-account"
    <> metavar "STRING"
    <> help "account name for transfer sender"
  <*< field @"mpt_rcvAccount" .:: option auto
    % long "receiver-account"
    <> metavar "STRING"
    <> help "account name for transfer recipient"
  <*< field @"mpt_batchSize" .:: option auto
    % long "batch-size"
    <> short 'b'
    <> metavar "INT"
    <> help "Number of transactions to bundle into a single 'send' call"
  <*< field @"mpt_confirmationDepth" .:: option auto
    % long "confirmation-depth"
    <> metavar "INT"
    <> help "confirmation depth"
  <*< field @"mpt_verbose" .:: option auto
      % long "verbose"
      <> metavar "BOOL"
      <> help "Whether to print out details of each transaction in a 'send' call"
  <*< field @"mpt_gasLimit" .:: option parseGasLimit
      % long "gasLimit"
      <> metavar "INT"
      <> help "The gas limit of each auto-generated transaction"
  <*< field @"mpt_gasPrice" .:: option parseGasPrice
      % long "gasPrice"
      <> metavar "DECIMAL"
      <> help "The gas price of each auto-generated transaction"
  <*< field @"mpt_timetolive" .:: option parseTTL
      % long "time-to-live"
      <> metavar "SECONDS"
      <> help "The time to live (in seconds) of each auto-generated transaction"
  <*< field @"mpt_hostAddresses" %:: pLeftSemigroupalUpdate (pure <$> pHostAddress' Nothing)
  <*< field @"mpt_nodeVersion" .:: textOption chainwebVersionFromText
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  where
    parseGasLimit =
        GasLimit . ParsedInteger <$> read' @Integer "Cannot read gasLimit."
    parseGasPrice =
        GasPrice . ParsedDecimal <$> read' @Decimal "Cannot read gasPrice."
    parseTTL =
        TTLSeconds . ParsedInteger <$> read' @Integer "Cannot read time-to-live."

read' :: Read a => String -> ReadM a
read' msg = eitherReader (bimap (const msg) id . readEither)

pHostAddress' :: Maybe String -> O.Parser HostAddress
pHostAddress' service = HostAddress <$> pHostname service <*> pPort service
  where
    pHostname s = option (eitherReader (either f return . hostnameFromText . T.pack))
      % long (maybe "" (<> "-") s <> "hostname")
      <> help ("hostname" <> maybe "" (" for " <>) s)
    pPort s = option (eitherReader (either f return . portFromText . T.pack))
      % long (maybe "" (<> "-") s <> "port")
      <> help ("port number" <> maybe "" (" for " <>) s)
    f e = Left $ case fromException e of
      Just( TextFormatException err) -> T.unpack err
      _ -> displayException e

data Args = Args
  { scriptCommand   :: !TXCmd
  , nodeChainIds    :: ![ChainId]
  , isChainweb      :: !Bool
  , hostAddresses   :: ![HostAddress]
  , nodeVersion     :: !ChainwebVersion
  , batchSize       :: !BatchSize
  , verbose         :: !Verbose
  , gasLimit        :: GasLimit
  , gasPrice        :: GasPrice
  , timetolive      :: TTLSeconds
  } deriving (Show, Generic)

instance ToJSON Args where
  toJSON o = object
    [ "scriptCommand"   .= scriptCommand o
    , "nodeChainIds"    .= nodeChainIds o
    , "isChainweb"      .= isChainweb o
    , "hostAddresses"   .= hostAddresses o
    , "chainwebVersion" .= nodeVersion o
    , "batchSize"       .= batchSize o
    , "verbose"         .= verbose o
    , "gasLimit"        .= gasLimit o
    , "gasPrice"        .= gasPrice o
    , "timetolive"      .= timetolive o
    ]

instance FromJSON (Args -> Args) where
  parseJSON = withObject "Args" $ \o -> id
    <$< field @"scriptCommand"   ..: "scriptCommand"   % o
    <*< field @"nodeChainIds"    ..: "nodeChainIds"    % o
    <*< field @"isChainweb"      ..: "isChainweb"      % o
    <*< field @"hostAddresses"   ..: "hostAddresses"   % o
    <*< field @"nodeVersion"     ..: "chainwebVersion" % o
    <*< field @"batchSize"       ..: "batchSize"       % o
    <*< field @"verbose"         ..: "verbose"         % o
    <*< field @"gasLimit"        ..: "gasLimit"        % o
    <*< field @"gasPrice"        ..: "gasPrice"        % o
    <*< field @"timetolive"      ..: "timetolive"      % o

defaultArgs :: Args
defaultArgs = Args
  { scriptCommand   = RunSimpleExpressions defaultTimingDist
  , nodeChainIds    = []
  , isChainweb      = True
  , hostAddresses   = []
  , nodeVersion     = v
  , batchSize       = BatchSize 1
  , verbose         = Verbose False
  , gasLimit = Sim.defGasLimit
  , gasPrice = Sim.defGasPrice
  , timetolive = Sim.defTTL
  }
  where
    v :: ChainwebVersion
    v = Development

scriptConfigParser :: MParser Args
scriptConfigParser = id
  <$< field @"scriptCommand" .:: textOption transactionCommandFromText
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll', 'listen', 'transfers', and 'simple'.")
  <*< field @"nodeChainIds" %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< field @"hostAddresses" %:: pLeftSemigroupalUpdate (pure <$> pHostAddress' Nothing)
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
  where
    parseGasLimit =
        GasLimit . ParsedInteger <$> read' @Integer "Cannot read gasLimit."
    parseGasPrice =
        GasPrice . ParsedDecimal <$> read' @Decimal "Cannot read gasPrice."
    parseTTL =
        TTLSeconds . ParsedInteger <$> read' @Integer "Cannot read time-to-live."
    pChainId = textOption cidFromText
      % long "node-chain-id"
      <> short 'i'
      <> metavar "INT"
      <> help "The specific chain that will receive generated transactions. Can be used multiple times."

instance ToJSON Hostname where
    toJSON = toJSON . hostnameToText
    {-# INLINE toJSON #-}

instance FromJSON Hostname where
    parseJSON = withText "Hostname" $! either fail return . fromText
      where
        fromText = either f return . hostnameFromText
        f e = Left $ case fromException e of
          Just (TextFormatException err) -> T.unpack err
          _ -> displayException e

    {-# INLINE parseJSON #-}

instance ToJSON HostAddress where
   toJSON o = object
      [ "hostname" .= _hostAddressHost o
      , "port" .= _hostAddressPort o
      ]
   {-# INLINE toJSON #-}

instance FromJSON HostAddress where
    parseJSON = withObject "HostAddress" $ \o -> HostAddress
      <$> o .: "hostname"
      <*> o .: "port"
    {-# INLINE parseJSON #-}

instance ToJSON Port
instance FromJSON Port

------------
-- TXG Monad
------------

-- TODO: Ideally we'd shove `LoggerT` into this stack, but `yet-another-logger`
-- would have to be patched to add missing instances first. Having `LoggerT`
-- here would let us remove the `MonadTrans` instance, as well as a number of
-- `lift` calls.

-- | The principal application Monad for this Transaction Generator.
newtype TXG m a = TXG { runTXG :: ReaderT TXGConfig (StateT TXGState m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState TXGState, MonadReader TXGConfig)

instance MonadTrans TXG where
  lift = TXG . lift . lift

data TXGState = TXGState
  { gsGen     :: !(Gen (PrimState IO))
  , gsCounter :: !(TVar TXCount)
  , gsChains  :: !(NESeq ChainId)
  } deriving (Generic)

data TXGConfig = TXGConfig
  { confTimingDist :: !(Maybe TimingDistribution)
  , confKeysets :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))))
  , confHost :: !HostAddress
  , confManager :: !Manager
  , confVersion :: !ChainwebVersion
  , confBatchSize :: !BatchSize
  , confVerbose :: !Verbose
  , confGasLimit :: GasLimit
  , confGasPrice :: GasPrice
  , confTTL :: TTLSeconds
  } deriving (Generic)

mkTXGConfig :: Maybe TimingDistribution -> Args -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config hostAddr =
  TXGConfig mdistribution mempty
  <$> pure hostAddr
  <*> unsafeManager
  <*> pure (nodeVersion config)
  <*> pure (batchSize config)
  <*> pure (verbose config)
  <*> pure (gasLimit config)
  <*> pure (gasPrice config)
  <*> pure (timetolive config)

-- -------------------------------------------------------------------------- --
-- MISC

-- | A running count of all transactions handles over all threads.
newtype TXCount = TXCount Word
  deriving newtype (Num, Show)

newtype BatchSize = BatchSize Word
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq, Read, Show, ToJSON, FromJSON)

nelReplicate :: Word -> a -> NEL.NonEmpty a
nelReplicate n a = NEL.unfoldr f n
  where
    f 0 = error "nelReplicate: Can't have length-0 list."
    f 1 = (a, Nothing)
    f m = (a, Just $ m - 1)

nelZipWith3 :: (a -> b -> c -> d) -> NEL.NonEmpty a -> NEL.NonEmpty b -> NEL.NonEmpty c -> NEL.NonEmpty d
nelZipWith3 f ~(x NEL.:| xs) ~(y NEL.:| ys) ~(z NEL.:| zs) = f x y z NEL.:| zipWith3 f xs ys zs
