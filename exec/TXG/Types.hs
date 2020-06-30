{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import           BasePrelude hiding (loop, option, rotate, timeout, (%))
import           Configuration.Utils hiding (Error, Lens')
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Primitive
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Decimal
import           Data.Generics.Product.Fields (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import           Data.Sequence.NonEmpty (NESeq(..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Network.HostAddress
import           Network.HTTP.Client hiding (Proxy)
import           Pact.Parse
import           Pact.Types.ChainMeta
import           Pact.Types.Command (SomeKeyPairCaps)
import           Pact.Types.Gas
import           System.Random.MWC (Gen)
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
    read' :: Read a => String -> ReadM a
    read' msg = eitherReader (bimap (const msg) id . readEither)
    pChainId = textOption cidFromText
      % long "node-chain-id"
      <> short 'i'
      <> metavar "INT"
      <> help "The specific chain that will receive generated transactions. Can be used multiple times."

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
  , confKeysets :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName (NonEmpty SomeKeyPairCaps))))
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

nelReplicate :: Word -> a -> NonEmpty a
nelReplicate n a = NEL.unfoldr f n
  where
    f 0 = error "nelReplicate: Can't have length-0 list."
    f 1 = (a, Nothing)
    f m = (a, Just $ m - 1)

nelZipWith3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
nelZipWith3 f ~(x :| xs) ~(y :| ys) ~(z :| zs) = f x y z :| zipWith3 f xs ys zs
