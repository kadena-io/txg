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
  , MPTState(..)
  , TXGConfig(..), mkTXGConfig, mkConfig
    -- * Pact API
  , pactPost
  , ClientError(..)
  , unsafeManager
    -- * Misc.
  , TXCount(..)
  , Cut
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
import qualified Data.ByteString.Lazy as LB
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

pChainId :: O.Parser ChainId
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
newtype TXG s m a = TXG { runTXG :: ReaderT TXGConfig (StateT s m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState s, MonadReader TXGConfig)

instance MonadTrans (TXG s) where
  lift = TXG . lift . lift

data TXGState = TXGState
  { gsGen     :: !(Gen (PrimState IO))
  , gsCounter :: !(TVar TXCount)
  , gsChains  :: !(NESeq ChainId)
  } deriving (Generic)

type Cut = LB.ByteString

data MPTState = MPTState
  { mptGen    :: !(Gen (PrimState IO))
  , mptCounter :: !(TVar TXCount)
  , mptLatestCut :: !(TVar Cut)
  , mptChains :: !(NESeq ChainId)
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

mkConfig
  :: Maybe TimingDistribution
  -> ChainwebVersion
  -> BatchSize
  -> Verbose
  -> GasLimit
  -> GasPrice
  -> TTLSeconds
  -> HostAddress
  -> IO TXGConfig
mkConfig mdistribution version batchSize v gl gp ttl hostAddr =
  TXGConfig mdistribution mempty
  <$> pure hostAddr
  <*> unsafeManager
  <*> pure version
  <*> pure batchSize
  <*> pure v
  <*> pure gl
  <*> pure gp
  <*> pure ttl

mkTXGConfig :: Maybe TimingDistribution -> Args -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config hostAddr =
  mkConfig mdistribution
    (nodeVersion config)
    (batchSize config)
    (verbose config)
    (gasLimit config)
    (gasPrice config)
    (timetolive config)
    hostAddr

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
