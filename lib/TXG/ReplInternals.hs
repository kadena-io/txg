{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TXG.ReplInternals where

import           BasePrelude
import           Control.Lens hiding ((.=))
import           Control.Monad.State
import           Data.Aeson
import           Data.Decimal
import           Data.Default
import qualified Data.List.NonEmpty as NEL
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HostAddress
import           Network.HTTP.Client hiding (Proxy(..))
import           Pact.ApiReq
import           Pact.Types.API
import           Pact.Types.ChainId (NetworkId(..))
import           Pact.Types.ChainMeta
import           Pact.Types.Command
import           Pact.Types.Hash
import           System.Random
import           TXG.Simulate.Contracts.CoinContract
import           TXG.Simulate.Contracts.Common
import           TXG.Simulate.Contracts.HelloWorld
import           TXG.Simulate.Contracts.SimplePayments
import           TXG.Simulate.Utils
import           TXG.Utils

-- for ghci

mkNetwork :: ChainwebVersion -> HostAddress -> ChainId -> IO Network
mkNetwork v h cid = Network v h cid <$> unsafeManager

data Network = Network
  { networkVersion :: !ChainwebVersion
  , networkHost    :: !HostAddress
  , networkChainId :: !ChainId
  , networkManager :: !Manager
  }

pactPostNetwork
    :: ToJSON body
    => FromJSON result
    => Network
    -> T.Text
    -> body
    -> IO (Either ClientError result)
pactPostNetwork n pactPath body = pactPost
    (networkManager n)
    (networkHost n)
    (networkVersion n)
    (networkChainId n)
    pactPath
    body

pactSend
    :: Network
    -> [Command Text]
    -> IO (Either ClientError RequestKeys)
pactSend n xs = do
    putStrLn
        $ "Sending: version=" ++ show (networkVersion n)
        ++ ", cid=" ++ show (networkChainId n)
        ++ ", SubmitBatch=" ++ show (SubmitBatch(NEL.fromList xs))
    pactPostNetwork n "/send" (SubmitBatch (NEL.fromList xs))

pactPoll
    :: Network
    -> RequestKeys
    -> IO (Either ClientError PollResponses)
pactPoll n rkeys = pactPostNetwork n "/poll" $ Poll (_rkRequestKeys rkeys)

pactLocal
    :: Network
    -> Command Text
    -> IO (Either ClientError (CommandResult Hash))
pactLocal n cmdText = pactPostNetwork n "/local" cmdText

pactListen
    :: Network
    -> RequestKey
    -> IO (Either ClientError ListenResponse)
pactListen n rk = pactPostNetwork n "/listen" (ListenerRequest rk)

cmd
    :: String
    -- ^ Code
    -> Value
    -- ^ Env data
    -> PublicMeta
    -> [SomeKeyPairCaps]
    -> Maybe NetworkId
    -> Maybe String
    -- ^ Transaction nonce.  If Nothing, then getCurrentTime is used.
    -> IO (Command Text)
cmd = mkExec

cmdStr :: String -> IO (Command Text)
cmdStr str = cmd str Null defPubMeta [] Nothing Nothing

-- Structured transactions

data Builtin = Hello | Payments

type Account = String

type SenderName = String

type ReceiverName = String

type Balance = Decimal

type Amount = Double

data CallBuiltIn'
    = CC CoinContractRequest
    | SP SimplePaymentRequest (Maybe (NEL.NonEmpty SomeKeyPairCaps))
    | HelloCode Text

data TxContent
    = PactCode String
    | Define Builtin
    | CallBuiltin CallBuiltIn'

easyTxToCommand :: TxContent -> IO (Command Text)
easyTxToCommand txContent = do
    ks <- testSomeKeyPairs
    txToCommand defChainwebVersion defPubMeta ks txContent

txToCommand
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty SomeKeyPairCaps
    -> TxContent
    -> IO (Command Text)
txToCommand v pubmeta ks = \case
    PactCode str -> cmdStr str
    Define Hello -> helloWorldContractLoader v pubmeta ks
    Define Payments -> simplePaymentsContractLoader v pubmeta ks
    CallBuiltin (CC coinReq) -> createCoinContractRequest v pubmeta ks coinReq
    CallBuiltin (SP spReq mkeyset) -> simplePayReq v pubmeta spReq mkeyset
    CallBuiltin (HelloCode helloname) -> helloRequest v $ Name helloname

defChainwebVersion :: ChainwebVersion
defChainwebVersion = Development

defChainId :: ChainId
defChainId = foldr const err $ chainIds defChainwebVersion
  where
    err = error "You shouldn't have a chainweb version with 0 chains"

defPubMeta :: PublicMeta
defPubMeta = def
    & set pmChainId "0"
    & set pmSender "sender00"
    & set pmGasLimit 1000
    & set pmGasPrice 0.0000001
    & set pmTTL 28800


generateDefaultSimpleCommands :: Int -> IO [Command Text]
generateDefaultSimpleCommands batchsize =
    replicateM batchsize $ getStdRandom (runState go) >>= cmdStr
  where
    go = do
        a <- state $ randomR (1, 100 :: Integer)
        b <- state $ randomR (1, 100 :: Integer)
        opIndex <- state $ randomR (0, 2 :: Int)
        return $ printf "(%s %u %u)" ["+-*" !! opIndex] a b

