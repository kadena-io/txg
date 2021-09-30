{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

-- | Module: MPT.Types
-- Copyright: Copyright © 2018 - 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MPT.Types where

import           Configuration.Utils hiding (Error, Lens')
import           Control.Concurrent.STM
import           Control.Monad.Primitive
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Decimal
import           Data.Generics.Product.Fields (field)
import           Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Text as T
import           GHC.Generics
import           Network.HostAddress
import qualified Options.Applicative as O
import           Pact.Parse
import           Pact.Types.ChainMeta
import           Pact.Types.Gas
import           System.Random.MWC (Gen)
import           Text.Read (readEither)
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Types
import           TXG.Utils

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

data MPTArgs = MPTArgs
  {
    mpt_transferRate      :: !Int -- number of microseconds to wait before sending next coin contract transfer transaction
  , mpt_accounts          :: ![String]
  , mpt_batchSize         :: !BatchSize
  , mpt_confirmationDepth :: !Int
  , mpt_verbose           :: !Verbose
  , mpt_gasLimit          :: GasLimit
  , mpt_gasPrice          :: GasPrice
  , mpt_timetolive        :: TTLSeconds
  , mpt_hosts     :: ![ChainwebHost]
  , mpt_nodeVersion       :: !ChainwebVersion
  , mpt_nodeChainIds      :: [ChainId]
  , mpt_dbFile            :: !T.Text
  } deriving (Show, Generic)


instance ToJSON MPTArgs where
  toJSON o = object
    [
      "transferRate"      .= mpt_transferRate o
    , "accounts"          .= mpt_accounts o
    , "batchSize"         .= mpt_batchSize o
    , "confirmationDepth" .= mpt_confirmationDepth o
    , "verbose"           .= mpt_verbose o
    , "gasLimit"          .= mpt_gasLimit o
    , "gasPrice"          .= mpt_gasPrice o
    , "timetolive"        .= mpt_timetolive o
    , "hosts"             .= mpt_hosts o
    , "nodeVersion"       .= mpt_nodeVersion o
    , "nodeChainIds"      .= mpt_nodeChainIds o
    , "dbFile"            .= mpt_dbFile o
    ]

instance FromJSON (MPTArgs -> MPTArgs) where
  parseJSON = withObject "MPTArgs" $ \o -> id
    <$< field @"mpt_transferRate" ..: "transferRate" % o
    <*< field @"mpt_accounts" ..: "accounts" % o
    <*< field @"mpt_batchSize" ..: "batchSize" % o
    <*< field @"mpt_confirmationDepth" ..: "confirmationDepth" % o
    <*< field @"mpt_verbose" ..: "verbose" % o
    <*< field @"mpt_gasLimit" ..: "gasLimit" % o
    <*< field @"mpt_gasPrice" ..: "gasPrice" % o
    <*< field @"mpt_timetolive" ..: "timetolive" % o
    <*< field @"mpt_hosts" ..: "hosts" % o
    <*< field @"mpt_nodeVersion" ..: "nodeVersion" % o
    <*< field @"mpt_nodeChainIds" ..: "nodeChainIds" % o
    <*< field @"mpt_dbFile" ..: "dbFile" % o

defaultMPTArgs :: MPTArgs
defaultMPTArgs = MPTArgs
  {
    mpt_transferRate      = 1_000_000
  , mpt_accounts          = ["sender01", "sender02"]
  , mpt_batchSize         = BatchSize 1
  , mpt_confirmationDepth = 6
  , mpt_verbose           = Verbose False
  , mpt_gasLimit          = Sim.defGasLimit
  , mpt_gasPrice          = Sim.defGasPrice
  , mpt_timetolive        = Sim.defTTL
  , mpt_hosts     = mempty
  , mpt_nodeVersion       = Development
  , mpt_nodeChainIds       = []
  , mpt_dbFile          = "mpt-data.sql"
  }

mpt_scriptConfigParser :: MParser MPTArgs
mpt_scriptConfigParser = id
  <$< field @"mpt_transferRate" .:: option auto
    % long "transfer-rate"
    <> metavar "INT"
    <> help "Number of microseconds to wait before sending next coin contract transfer transaction"
  <*< field @"mpt_accounts" %:: pLeftSemigroupalUpdate (pure <$> pAccount)
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
  <*< field @"mpt_hosts" %:: pLeftSemigroupalUpdate (pure <$> pChainwebHost)
  <*< field @"mpt_nodeChainIds" %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< field @"mpt_nodeVersion" .:: textOption chainwebVersionFromText
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  <*< field @"mpt_dbFile" .:: strOption
    % long "db-file"
    <> metavar "FILEPATH"
    <> help "File name for sqlite database."
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

pAccount :: O.Parser String
pAccount = strOption
  % long "account"
  <> metavar "STRING"
  <> help "account name for transfers"

pChainId :: O.Parser ChainId
pChainId = textOption cidFromText
  % long "node-chain-id"
  <> short 'i'
  <> metavar "INT"
  <> help "The specific chain that will receive generated transactions. Can be used multiple times."

data ErrorType = RateLimiting | ClientError | ServerError | OtherError T.Text
  deriving (Eq,Ord,Show)

type Cut = LB.ByteString

data MPTState = MPTState
  { mptGen    :: !(Gen (PrimState IO))
  , mptCounter :: !(TVar TXCount)
  , mptLatestCut :: !(TVar Cut)
  , mptChains :: !(NESeq ChainId)
  } deriving (Generic)
