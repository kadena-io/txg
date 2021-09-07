{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Bifunctor
import           Data.Decimal
import           Data.Generics.Product.Fields (field)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.HostAddress
import qualified Options.Applicative as O
import           Pact.Parse
import           Pact.Types.ChainMeta
import           Pact.Types.Gas
import           Text.Read (readEither)
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Types
import           TXG.Utils

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
  , mpt_hostAddresses     :: ![HostAddress]
  , mpt_nodeVersion       :: !ChainwebVersion
  , mpt_nodeChainIds      :: [ChainId]
  , mpt_dbFile            :: FilePath
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
    , "nodeChainIds"      .= mpt_nodeChainIds o
    , "dbFile"            .= mpt_dbFile o
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
    <*< field @"mpt_nodeChainIds" ..: "mpt_nodeChainIds" % o
    <*< field @"mpt_dbFile" ..: "mpt_dbFile" % o

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
  , mpt_nodeChainIds       = []
  , mpt_dbFile          = "mpt-data.sql"
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
  <*< field @"mpt_nodeChainIds" %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< field @"mpt_nodeVersion" .:: textOption chainwebVersionFromText
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  <*< field @"mpt_dbFile" .:: option auto
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

pChainId :: O.Parser ChainId
pChainId = textOption cidFromText
  % long "node-chain-id"
  <> short 'i'
  <> metavar "INT"
  <> help "The specific chain that will receive generated transactions. Can be used multiple times."

data ErrorType = RateLimiting | ClientError | ServerError | OtherError T.Text
  deriving (Eq,Ord,Show)
