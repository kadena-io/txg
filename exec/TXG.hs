{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Module: TXG
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Main ( main ) where

import           Configuration.Utils hiding (option, many, Error, Lens')
import           Control.Concurrent
import           Control.Concurrent.Async hiding (poll)
import           Control.Concurrent.STM
import           Control.Exception (throwIO)
import           Control.Lens hiding (op, (.=), (|>))
import           Control.Monad
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Except
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import           Control.Retry
import           Data.Aeson.Key (fromText)
import           Data.Aeson.Lens
import           Data.Generics.Product.Fields (field)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SB
import           Data.Either
import           Data.Foldable
import           Data.Functor.Compose
-- import qualified Data.List as L
import           Data.Int
import           Data.List.Split (chunksOf)
import qualified Data.List.NonEmpty as NEL
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import           Data.String.Conv (toS)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           Network.HostAddress
import           Network.HTTP.Client hiding (host)
import           Network.HTTP.Types
import           Pact.ApiReq
import qualified Pact.JSON.Encode as J
import           Pact.Types.API
import           Pact.Types.Capability
import qualified Pact.Types.ChainId as CI
import qualified Pact.Types.ChainMeta as CM
import           Pact.Types.Command
import           Pact.Types.Exp (Literal(..))
import           Pact.Types.Gas
import qualified Pact.Types.Hash as H
import           Pact.Types.Info (mkInfo)
import           Pact.Types.Names
import           Pact.Types.PactError
import           Pact.Types.PactValue
import           System.Logger hiding (StdOut)
import qualified System.Logger as Y
import           System.Exit
import           System.Random
import           System.Random.MWC (createSystemRandom, uniformR)
import           System.Random.MWC.Distributions (normal)
import           Text.Pretty.Simple (pPrintNoColor)
import           Text.Printf
import           TXG.Fake (fake, generate)
import           TXG.Simulate.Contracts.CoinContract
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Simulate.Contracts.HelloWorld
import           TXG.Simulate.Contracts.SimplePayments
import           TXG.Simulate.Utils
import           TXG.Utils
import           TXG.Types

import qualified Network.HTTP.Client as HTTP
-- import qualified Network.HTTP.Client.OpenSSL as HTTP
-- import qualified Network.HTTP.Types.Header as HTTP
-- import qualified Network.HTTP.Types.Method as HTTP

---

generateDelay :: MonadIO m => TXG TXGState m Int
generateDelay = do
  distribution <- asks confTimingDist
  gen <- gets gsGen
  case distribution of
    Just (GaussianTD (Gaussian gmean gvar)) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (UniformTD (Uniform ulow uhigh)) -> liftIO (truncate <$> System.Random.MWC.uniformR (ulow, uhigh) gen)
    Nothing -> error "generateDelay: impossible"

generateSimpleTransactions
  :: (MonadIO m, MonadLog T.Text m)
  => TXG TXGState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
generateSimpleTransactions = do
  -- Choose a Chain to send these transactions to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate
  -- Generate a batch of transactions
  stdgen <- liftIO newStdGen
  BatchSize batch <- asks confBatchSize
  v <- asks confVersion
  tGasLimit <- asks confGasLimit
  tGasPrice <- asks confGasPrice
  tTTL <- asks confTTL
  (msgs, cmds) <- liftIO . fmap NEL.unzip . sequenceA . nelReplicate batch $ f tGasLimit tGasPrice tTTL cid v stdgen
  -- Delay, so as not to hammer the network.
  delay <- generateDelay
  liftIO $ threadDelay delay
  pure (cid, msgs, cmds)
 where
    f
        :: GasLimit
        -> GasPrice
        -> CM.TTLSeconds
        -> Sim.ChainId
        -> ChainwebVersion
        -> StdGen
        -> IO (Maybe Text, Command Text)
    f gl gp ttl cid v stdgen = do
      let (operandA, operandB, op) = flip evalState stdgen $ do
            a <- state $ randomR (1, 100 :: Integer)
            b <- state $ randomR (1, 100 :: Integer)
            ind <- state $ randomR (0, 2 :: Int)
            let operation = "+-*" !! ind
            pure (a, b, operation)
          theCode = "(" ++ [op] ++ " " ++ show operandA ++ " " ++ show operandB ++ ")"

      -- this contains the key of sender00
      kps <- testDynKeyPairs

      let theData = object ["test-admin-keyset" .= fmap (formatPubKeyForCmd . fst) kps]
      meta <- Sim.makeMeta cid ttl gp gl
      (Nothing,)
        <$> mkExec (T.pack theCode) theData meta
            (NEL.toList kps)
            (Just $ CI.NetworkId $ chainwebVersionToText v)
            Nothing

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

data CmdChoice = CoinContract | HelloWorld | Payments
  deriving (Show, Eq, Ord, Bounded, Enum)

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = toEnum <$> randomRIO @Int (0, fromEnum $ maxBound @a)

_generateXChainTransactions
    :: forall m. (MonadIO m, MonadLog T.Text m)
    => Verbose
    -> TXG TXGState m (Sim.ChainId, Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
_generateXChainTransactions isVerbose = do
  -- Choose a source Chain to send this transaction to, and cycle the state.
  sourceChain <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate

  -- Choose a target Chain to send this transaction to, and cycle the state.
  targetChain <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate

  cks <- asks confKeysets
  version <- asks confVersion
  case M.lookup sourceChain cks of
    Nothing -> error $ printf "Source Chain (%s) is missing Accounts!\n" (show sourceChain)
    Just accs -> do
      BatchSize batch <- asks confBatchSize
      tGasLimit <- asks confGasLimit
      tGasPrice <- asks confGasPrice
      tTTL <- asks confTTL
      (mmsgs, cmds) <- liftIO . fmap NEL.unzip . sequenceA . nelReplicate batch $
          xChainTransfer tGasLimit tGasPrice tTTL version isVerbose sourceChain targetChain $ accounts "coin" accs
      generateDelay >>= liftIO . threadDelay
      pure (sourceChain, targetChain, mmsgs, cmds)
  where
    accounts :: String -> Map Sim.Account (Map Sim.ContractName a) -> Map Sim.Account a
    accounts s = fromJuste . traverse (M.lookup (Sim.ContractName s))

    xChainTransfer
        :: GasLimit
        -> GasPrice
        -> CM.TTLSeconds
        -> ChainwebVersion
        -> Verbose
        -> Sim.ChainId
        -> Sim.ChainId
        -> Map Sim.Account (NEL.NonEmpty (DynKeyPair,[SigCapability]))
        -> IO (Maybe Text, Command Text)
    xChainTransfer gl gp ttl version (Verbose vb) sourceChain _targetChain coinaccts = do
      coinContractRequest <- mkRandomCoinContractRequest True coinaccts >>= generate
      let msg = if vb then Just $ T.pack (show coinContractRequest) else Nothing
      let acclookup sn@(Sim.Account accsn) =
            case M.lookup sn coinaccts of
              Just ks -> (sn, ks)
              Nothing -> error $ "Couldn't find account: <" ++ accsn ++ ">"
      let (Sim.Account sender, ks) =
            case coinContractRequest of
              CoinCreateAccount account (Guard guardd) -> (account, guardd)
              CoinAccountBalance account -> acclookup account
              CoinTransfer (SenderName sn) rcvr amt ->
                mkTransferCaps rcvr amt $ acclookup sn
              CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt ->
                mkTransferCaps rcvr amt (acc, guardd)
      meta <- Sim.makeMetaWithSender sender ttl gp gl sourceChain
      (msg,) <$> createCoinContractRequest version meta ks coinContractRequest

    mkTransferCaps :: ReceiverName -> Sim.Amount -> (Sim.Account, NEL.NonEmpty (DynKeyPair,[SigCapability])) -> (Sim.Account, NEL.NonEmpty (DynKeyPair,[SigCapability]))
    mkTransferCaps (ReceiverName (Sim.Account r)) (Sim.Amount m) (s@(Sim.Account ss),ks) = (s, (caps <$) <$> ks)
      where caps = [gas,tfr]
            gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
            tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                  [ PLiteral $ LString $ T.pack ss
                  , PLiteral $ LString $ T.pack r
                  , PLiteral $ LDecimal m]

generateTransactions
    :: forall m. (MonadIO m, MonadLog T.Text m)
    => Bool
    -> Verbose
    -> CmdChoice
    -> TXG TXGState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
generateTransactions ifCoinOnlyTransfers isVerbose contractIndex = do
  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- NES.head <$> gets gsChains
  field @"gsChains" %= rotate

  cks <- asks confKeysets
  version <- asks confVersion
  case M.lookup cid cks of
    Nothing -> error $ printf "%s is missing Accounts!\n" (show cid)
    Just accs -> do
      BatchSize batch <- asks confBatchSize
      tGasLimit <- asks confGasLimit
      tGasPrice <- asks confGasPrice
      tTTL <- asks confTTL
      (mmsgs, cmds) <- liftIO . fmap NEL.unzip . sequenceA . nelReplicate batch $
        case contractIndex of
          CoinContract -> coinContract tGasLimit tGasPrice tTTL version ifCoinOnlyTransfers isVerbose cid $ accounts "coin" accs
          HelloWorld -> (Nothing,) <$> (generate fake >>= helloRequest version)
          Payments -> (Nothing,) <$> payments tGasLimit tGasPrice tTTL version cid (accounts "payment" accs)
      generateDelay >>= liftIO . threadDelay
      pure (cid, mmsgs, cmds)
  where
    accounts :: String -> Map Sim.Account (Map Sim.ContractName a) -> Map Sim.Account a
    accounts s = fromJuste . traverse (M.lookup (Sim.ContractName s))

    coinContract
        :: GasLimit
        -> GasPrice
        -> CM.TTLSeconds
        -> ChainwebVersion
        -> Bool
        -> Verbose
        -> Sim.ChainId
        -> Map Sim.Account (NEL.NonEmpty (DynKeyPair,[SigCapability]))
        -> IO (Maybe Text, Command Text)
    coinContract gl gp ttl version transfers (Verbose vb) cid coinaccts = do
      coinContractRequest <- mkRandomCoinContractRequest transfers coinaccts >>= generate
      let msg = if vb then Just $ T.pack (show coinContractRequest) else Nothing
      let acclookup sn@(Sim.Account accsn) =
            case M.lookup sn coinaccts of
              Just ks -> (sn, ks)
              Nothing -> error $ "Couldn't find account: <" ++ accsn ++ ">"
      let (Sim.Account sender, ks) =
            case coinContractRequest of
              CoinCreateAccount account (Guard guardd) -> (account, guardd)
              CoinAccountBalance account -> acclookup account
              CoinTransfer (SenderName sn) rcvr amt ->
                mkTransferCaps rcvr amt $ acclookup sn
              CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt ->
                mkTransferCaps rcvr amt (acc, guardd)
      meta <- Sim.makeMetaWithSender sender ttl gp gl cid
      (msg,) <$> createCoinContractRequest version meta ks coinContractRequest

    mkTransferCaps :: ReceiverName -> Sim.Amount -> (Sim.Account, NEL.NonEmpty (DynKeyPair,[SigCapability])) -> (Sim.Account, NEL.NonEmpty (DynKeyPair,[SigCapability]))
    mkTransferCaps (ReceiverName (Sim.Account r)) (Sim.Amount m) (s@(Sim.Account ss),ks) = (s, (caps <$) <$> ks)
      where caps = [gas,tfr]
            gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
            tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                  [ PLiteral $ LString $ T.pack ss
                  , PLiteral $ LString $ T.pack r
                  , PLiteral $ LDecimal m]

    payments :: GasLimit -> GasPrice -> CM.TTLSeconds -> ChainwebVersion -> Sim.ChainId -> Map Sim.Account (NEL.NonEmpty (DynKeyPair,[SigCapability])) -> IO (Command Text)
    payments gl gp ttl v cid paymentAccts = do
      paymentsRequest <- mkRandomSimplePaymentRequest paymentAccts >>= generate
      case paymentsRequest of
        SPRequestPay fromAccount _ _ -> case M.lookup fromAccount paymentAccts of
          Nothing ->
            error "This account does not have an associated keyset!"
          Just keyset -> do
            meta <- Sim.makeMeta cid ttl gp gl
            simplePayReq v meta paymentsRequest $ Just keyset
        SPRequestGetBalance _account -> do
          meta <- Sim.makeMeta cid ttl gp gl
          simplePayReq v meta paymentsRequest Nothing
        _ -> error "SimplePayments.CreateAccount code generation not supported"

pactSend
  :: TXGConfig
  -> Sim.ChainId
  -> NEL.NonEmpty (Command Text)
  -> IO (Either ClientError RequestKeys)
pactSend c cid cmds = send
  (confManager c)
  (confHost c)
  (confVersion c)
  cid
  (toList cmds)

pactPoll
  :: TXGConfig
  -> Sim.ChainId
  -> RequestKeys
  -> IO (Either ClientError PollResponses)
pactPoll c cid = poll
  (confManager c)
  (confHost c)
  (confVersion c)
  cid

pactListen
  :: TXGConfig
  -> Sim.ChainId
  -> RequestKey
  -> IO (Either ClientError ListenResponse)
pactListen c cid = listen
  (confManager c)
  (confHost c)
  (confVersion c)
  cid

pactSPV
    :: TXGConfig
    -> Sim.ChainId
    -> RequestKey
    -> IO  (Either ClientError Text)
pactSPV c targetChain = spv
  (confManager c)
  (confHost c)
  (confVersion c)
  targetChain

isMempoolMember
  :: TXGConfig
  -> Sim.ChainId
  -> TransactionHashes
  -> IO (Either ClientError [Bool])
isMempoolMember c cid = mempoolMember
  (confManager c)
  (confHost c)
  (confVersion c)
  cid

_xChainLoop
  :: (MonadIO m, MonadLog T.Text m)
  => TXG TXGState m (Sim.ChainId, Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
  -> TXG TXGState m ()
_xChainLoop f = forever $ do
  (sourceChain, targetChain, _msgs, transactions) <- f
  config <- ask
  requestKeys <- liftIO $ pactSend config sourceChain transactions

  case requestKeys of
    Left servantError ->
      lift . logg Error $ T.pack (show servantError)
    Right rks -> do
      -- countTV <- gets gsCounter
      -- batch <- asks confBatchSize
      -- liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
      -- count <- liftIO $ readTVarIO countTV
      -- lift . logg Info $ "Transaction count: " <> T.pack (show count)
      -- lift . logg Info $ "Transaction requestKey: " <> T.pack (show rk)
      -- forM_ (Compose msgs) $ \m ->
      --   lift . logg Info $ "Actual transaction: " <> m
    -- there are some more steps
      void $ do
    -- 2) with request key, poll nodes for continuation on source chain
        conts <- liftIO (pactPoll config sourceChain rks)
                 >>= \case
                    Left _cerr -> undefined
                    Right (PollResponses polls) ->
                       forM polls $ \cr ->
                          return $ case _crContinuation cr of
                            Nothing -> Left ("Result was not a continuation\n" <> (show (_crResult cr)))
                            Just pe -> Right (_crReqKey cr, pe)

    -- 3) get spv proof with okCont from source chain
        _proofs <-  liftIO $ forM (rights $ toList conts) $ \(rk,pe) ->
          pactSPV config targetChain rk >>= \case
            Left err -> return $ Left $ show err
            Right proof -> return $ Right (pe, proof)
        -- proofs :: (NEL.NonEmpty Text) <- ExceptT $ liftIO $ fmap sequence $ mapM (\(rk, pe) -> pactSPV config targetChain rk) conts
    -- assume gas payer is same as sender
    -- 4) run continuation from spv proof on target chain
        let _sender = undefined
            _meta = undefined
        _payloads <- undefined
        _xconts <- undefined
        _xRequestKeys <- liftIO $ pactSend config targetChain $ _xconts
        countTV <- gets gsCounter
        batch <- asks confBatchSize
        liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
        cnt <- liftIO $ readTVarIO countTV
        lift . logg Info $ "Transaction count: " <> T.pack (show cnt)
        lift . logg Info $ "Transaction request keys: " <> T.pack (show _xRequestKeys)



loop
  :: (MonadIO m, MonadReader (Logger Text) m, MonadLog T.Text m)
  => Int
  -> TVar Cut
  -> TXG TXGState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
  -> TXG TXGState m ()
loop confDepth tcut f = forever $ do
    (cid, msgs, transactions) <- f
    config <- ask
    (requestKeys,start,end) <- liftIO $ trackTime $ pactSend config cid transactions

    case requestKeys of
      Left servantError ->
        lift . logg Error $ T.pack (show servantError)
      Right rks ->
        case confElasticSearchConfig config of
          Just esConf -> sendToElasticSearch esConf (confVersion config) start end rks
          Nothing -> do
            countTV <- gets gsCounter
            batch <- asks confBatchSize
            liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
            cnt <- liftIO $ readTVarIO countTV
            lift . logg Info $ "Transaction count: " <> T.pack (show cnt)
            lift . logg Info $ "Transaction requestKey: " <> T.pack (show rks)
            case confTrackMempoolStat config of
              Nothing -> pure ()
              Just p -> do
                let retrier = retrying policy (const (pure . isLeft)) . const
                    policy :: Monad m => RetryPolicyM m
                    policy =
                      exponentialBackoff (pollExponentialBackoffInitTime p)
                      <> limitRetries (pollRetries p)
                    toChunker = toList . _rkRequestKeys
                forM_ (chunksOf (pollChunkSize p) $ toChunker rks) $ \chunk -> do
                  poll_result <- lift $ retrier $ pollRequestKeys' config cid (RequestKeys $ NEL.fromList chunk)
                  case poll_result of
                    Left err -> lift $ logg Error $ T.pack $ printf "Caught this error while polling for these request keys (%s) %s" (show $ RequestKeys $ NEL.fromList  chunk) err
                    Right (poll_start,poll_end,result) -> iforM_ result $ \rk res ->  do
                      logStat cid rk (TimeUntilMempoolAcceptance (TimeSpan {start_time = start, end_time = end}))
                      logStat cid rk (TimeUntilBlockInclusion (TimeSpan {start_time = poll_start, end_time = poll_end}))
                      let h = fromIntegral $ fromJuste $ res ^? _2 . key "blockHeight" . _Integer
                      cstart <- liftIO getCurrentTimeInt64
                      logger' <- lift ask
                      liftIO $ (flip withAsync) wait $ do
                        cend <- loopUntilConfirmationDepth confDepth cid h tcut
                        logStatIO logger' cid rk (TimeUntilConfirmationDepth (TimeSpan {start_time = cstart, end_time = cend}))

                forM_ (Compose msgs) $ \m ->
                  lift . logg Info $ "Actual transaction: " <> m

sendToElasticSearch :: MonadIO m => ElasticSearchConfig -> ChainwebVersion -> Int64 -> Int64 -> RequestKeys -> TXG TXGState m ()
sendToElasticSearch esConf version start end rks = do
    esReq <- liftIO $ mkElasticSearchRequest esConf version start end rks
    mgr <- asks confManager
    liftIO $ httpJson mgr esReq

mkElasticSearchRequest :: MonadIO m => MonadThrow m => ElasticSearchConfig -> ChainwebVersion -> Int64 -> Int64 -> RequestKeys -> m HTTP.Request
mkElasticSearchRequest esConf version start end rks = do
    req <- HTTP.parseUrlThrow $ printf "%s:%d" (T.unpack $ hostnameToText $ esHost esConf) (show $ esPort esConf)
    currentTime <- liftIO $ getCurrentTimeInt64
    return req
      { HTTP.method = "POST"
      , HTTP.path = "/" <> toS idxName <> "/_bulk"
      , HTTP.responseTimeout = HTTP.responseTimeoutMicro 5_000_000
      , HTTP.checkResponse = HTTP.throwErrorStatusCodes
      , HTTP.requestHeaders =
        ("Content-Type", "application/x-ndjson") :
        maybe [] (\k -> [("Authoriation", "ApiKey " <> T.encodeUtf8 k)]) (esApiKey esConf)
      , HTTP.requestBody = body currentTime
      }
  where
    idxName :: String
    idxName = printf "chainweb-%s-%s" (show version) (show $ esIndex esConf)
    body now = HTTP.RequestBodyLBS $ mkItem $ mkElasticSearchPayload start end rks now
    mkItem x = "{\"index\":{}}" <> "\n" <> encode x <> "\n"
    mkElasticSearchPayload :: Int64 -> Int64 -> RequestKeys -> Int64 -> Value
    mkElasticSearchPayload s e rs now = object
      [ "batch-submission-time" .= s
      , "batch-confirmation-time" .= e
      , "requestKeys" .= J.toJsonViaEncode rs
      , "chainwebVersion" .= version
      , "timestamp" .= now
      ]

logStatIO :: Logger Text -> ChainId -> RequestKey -> MempoolStat' -> IO ()
logStatIO logger cid rk ms =
  loggerFunIO logger Info $ T.pack $ show $ MempoolStat
    {
      ms_chainid = cid
    , ms_txhash = rk
    , ms_stat = ms
    }

logStat :: (MonadTrans t, MonadLog Text m) => ChainId -> RequestKey -> MempoolStat' -> t m ()
logStat cid rk ms = lift $ logg Info $ T.pack $ show $ MempoolStat
  {
    ms_chainid = cid
  , ms_txhash = rk
  , ms_stat = ms
  }

type BlockHeight = Int

loopUntilConfirmationDepth :: Int -> ChainId -> BlockHeight -> TVar Cut -> IO Int64
loopUntilConfirmationDepth confDepth cid startHeight tcut = do
  atomically $ do
    cut <- readTVar tcut
    let height = fromIntegral $ fromJuste $ cut ^? key (fromText "hashes") . key (fromText $ cidToText cid) . key (fromText "height") . _Integer
    check (height >= (startHeight + confDepth))
  getCurrentTimeInt64

data MempoolStat = MempoolStat
  {
    ms_chainid :: ChainId
  , ms_txhash :: RequestKey
  , ms_stat :: MempoolStat'
  }

instance Show MempoolStat where
  show (MempoolStat cid txhash stat) = printf "chainid: %s, txhash: %s, data: %s\n" (show cid) (show txhash) (show stat)

data TimeSpan = TimeSpan
  {
    start_time :: Int64
  , end_time :: Int64
  }
instance Show TimeSpan where
  show (TimeSpan s e) = show (s,e)

data MempoolStat' =
  TimeUntilMempoolAcceptance TimeSpan
  | TimeUntilBlockInclusion TimeSpan
  | TimeUntilConfirmationDepth TimeSpan

instance Show MempoolStat' where
  show = show . \case
    TimeUntilMempoolAcceptance t -> t
    TimeUntilBlockInclusion t -> t
    TimeUntilConfirmationDepth t -> t

getCurrentTimeInt64 :: IO Int64
getCurrentTimeInt64 = do
  -- returns POSIX seconds with picosecond precision
  t <- getPOSIXTime
  return $! round $ t * 100_000

trackTime :: IO a -> IO (a, Int64, Int64)
trackTime act = do
  t1 <- getCurrentTimeInt64
  r <- act
  t2 <- getCurrentTimeInt64
  return (r,t1,t2)

pollRequestKeys' :: (MonadIO m, MonadLog T.Text m) => TXGConfig -> ChainId -> RequestKeys -> m (Either String (Int64, Int64, (HM.HashMap RequestKey (Maybe PactError, Value))))
pollRequestKeys' cfg cid rkeys = do
    (response, start, end) <- liftIO $ trackTime $ pactPoll cfg cid rkeys
    case response of
      Left _ -> pure $ Left "Failure"
      Right (PollResponses as)
        | null as -> do
          let zipper cs bs g = zipWith g cs bs
              rkList = NEL.toList $ _rkRequestKeys rkeys
              pollList = HM.elems as
              msg = unlines $ zipper rkList pollList $ \rk a ->
                  printf "Failure no result returned for request key (%s); error: %s" (show rk) (show a)
          pure $ Left $ "Failure no result returned: error: " <> msg
        | otherwise -> pure $ Right $ (start, end, fmap f as)
  where
    f cr = (either Just (const Nothing) $ _pactResult $ _crResult cr, fromJuste $ _crMetaData cr)

type ContractLoader
    = CM.PublicMeta -> NEL.NonEmpty (DynKeyPair,[SigCapability]) -> IO (Command Text)

loadContracts :: Args -> ChainwebHost -> NEL.NonEmpty ContractLoader -> IO ()
loadContracts config (ChainwebHost h _p2p service) contractLoaders = do
  conf@(TXGConfig _ _ _ _ _ _ (Verbose vb) tgasLimit tgasPrice ttl' _trackMempoolStat _elasticSearchConfig)
        <- mkTXGConfig Nothing config (HostAddress h service)
  forM_ (nodeChainIds config) $ \cid -> do
    !meta <- Sim.makeMeta cid ttl' tgasPrice tgasLimit
    ts <- testDynKeyPairs
    contracts <- traverse (\f -> f meta ts) contractLoaders
    pollresponse <- runExceptT $ do
      rkeys <- ExceptT $ pactSend conf cid contracts
      when vb
        $ withConsoleLogger Info
        $ logg Info
        $ "sent contracts with request key: " <> T.pack (show rkeys)
      ExceptT $ pactPoll conf cid rkeys
    withConsoleLogger Info . logg Info $ T.pack (show pollresponse)

queryCut :: Manager -> HostAddress -> ChainwebVersion -> IO (Either ApiError Cut)
queryCut mgr (HostAddress h p) version = do
  let url = "https://" <> hostnameToText h <> ":" <> portToText p <> "/chainweb/0.0/" <> chainwebVersionToText version <> "/cut"
  req <- parseRequest $ T.unpack url
  res <- handleRequest req mgr
  pure $ responseBody <$> res

handleRequest :: Request -> Manager -> IO (Either ApiError (Response LB.ByteString))
handleRequest req mgr = do
  res <- httpLbs req mgr
  let mkErr t = ApiError t (responseStatus res) (responseBody res)
      checkErr s
        | statusCode s == 429 || statusCode s == 403 = Left $ mkErr RateLimiting
        | statusIsClientError s = Left $ mkErr EClientError
        | statusIsServerError s = Left $ mkErr ServerError
        | statusIsSuccessful s = Right res
        | otherwise = Left $ mkErr $ OtherError "unknown error"
  pure $ checkErr (responseStatus res)

data ErrorType = RateLimiting | EClientError | ServerError | OtherError T.Text
  deriving (Eq,Ord,Show)

type Cut = LB.ByteString

data ApiError = ApiError
  { apiError_type :: ErrorType
  , apiError_status :: Status
  , apiError_body :: LB.ByteString
  } deriving (Eq,Ord,Show)

realTransactions
  :: Args
  -> ChainwebHost
  -> TVar Cut
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
realTransactions config (ChainwebHost h _p2p service) tcut tv distribution = do
  cfg@(TXGConfig _ _ _ _ v _ _ tgasLimit tgasPrice ttl' _trackMempoolStat _elasticSearchConfig)
        <- liftIO $ mkTXGConfig (Just distribution) config (HostAddress h service)

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    !meta <- liftIO $ Sim.makeMeta cid ttl' tgasPrice tgasLimit
    (paymentKS, paymentAcc) <- liftIO $ NEL.unzip <$> Sim.createPaymentsAccounts v meta
    (coinKS, coinAcc) <- liftIO $ NEL.unzip <$> Sim.createCoinAccounts v meta
    pollResponseCoin <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ pactSend cfg cid coinAcc
      ExceptT $ pactPoll cfg cid rkeys
    pollResponsePayment <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ pactSend cfg cid paymentAcc
      ExceptT $ pactPoll cfg cid rkeys
    case pollResponseCoin of
      Left e  -> do
        logg Error $ "Couldn't create coin-contract accounts"
        logg Error $ T.pack (show e)
      Right _ -> logg Info "Created coin-contract accounts"
    case pollResponsePayment of
      Left e  -> do
        logg Warn $ "Couldn't create payment-contract accounts (contract likely is not loaded)"
        logg Warn $ T.pack (show e)
      Right _ -> logg Info "Created payment-contract accounts"
    let accounts = buildGenAccountsKeysets Sim.accountNames paymentKS coinKS
    pure (cid, accounts)

  logg Info "Real Transactions: Transactions are being generated"

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let act = loop (confirmationDepth config) tcut (liftIO randomEnum >>= generateTransactions False (verbose config))
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NEL.NonEmpty Sim.Account
      -> NEL.NonEmpty (NEL.NonEmpty (DynKeyPair,[SigCapability]))
      -> NEL.NonEmpty (NEL.NonEmpty (DynKeyPair,[SigCapability]))
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    buildGenAccountsKeysets accs pks cks =
      M.fromList . NEL.toList $ nelZipWith3 go accs pks cks

    go :: Sim.Account
       -> NEL.NonEmpty (DynKeyPair,[SigCapability])
       -> NEL.NonEmpty (DynKeyPair,[SigCapability])
       -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

_realXChainCoinTransactions
  :: Args
  -> ChainwebHost
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
_realXChainCoinTransactions config (ChainwebHost h _p2p service) tv distribution = do
    when (null $ drop 1 $ nodeChainIds config) $ liftIO $ fail "You must specify at least 2 chains for cross-chain transfers"
    cfg <- liftIO $ mkTXGConfig (Just distribution) config (HostAddress h service)
    let chains = maybe (versionChains $ nodeVersion config) NES.fromList
                . NEL.nonEmpty
                $ nodeChainIds config
    accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
      let f (Sim.Account sender) = do
            meta <- liftIO $ Sim.makeMetaWithSender sender (confTTL cfg) (confGasPrice cfg) (confGasLimit cfg) cid
            Sim.createCoinAccount (confVersion cfg) meta sender
      (coinKS, _coinAcc) <-
          liftIO $ unzip <$> traverse f Sim.coinAccountNames
      let accounts = buildGenAccountsKeysets (NEL.fromList Sim.coinAccountNames) (NEL.fromList coinKS)
      pure (cid, accounts)

    logg Info "Real Transactions: Transactions are being generated"

    -- Set up values for running the effect stack.
    gen <- liftIO createSystemRandom
    let act = _xChainLoop (_generateXChainTransactions (verbose config))
        env = set (field @"confKeysets") accountMap cfg
        stt = TXGState gen tv chains

    evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NEL.NonEmpty Sim.Account
      -> NEL.NonEmpty (NEL.NonEmpty (DynKeyPair,[SigCapability]))
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    buildGenAccountsKeysets accs cks =
      M.fromList . NEL.toList $ NEL.zipWith go accs cks

    go :: Sim.Account
       -> NEL.NonEmpty (DynKeyPair,[SigCapability])
       -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    go name cks = (name, M.singleton (Sim.ContractName "coin") cks)

realCoinTransactions
  :: Args
  -> ChainwebHost
  -> TVar Cut
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
realCoinTransactions config (ChainwebHost h _p2p service) tcut tv distribution = do
  cfg <- liftIO $ mkTXGConfig (Just distribution) config (HostAddress h service)

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    let f (Sim.Account sender) = do
          meta <- liftIO $ Sim.makeMetaWithSender sender (confTTL cfg) (confGasPrice cfg) (confGasLimit cfg) cid
          Sim.createCoinAccount (confVersion cfg) meta sender
    (coinKS, _coinAcc) <-
        liftIO $ unzip <$> traverse f Sim.coinAccountNames
    let accounts = buildGenAccountsKeysets (NEL.fromList Sim.coinAccountNames) (NEL.fromList coinKS)
    pure (cid, accounts)

  logg Info "Real Transactions: Transactions are being generated"

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let act = loop (confirmationDepth config) tcut (generateTransactions True (verbose config) CoinContract)
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NEL.NonEmpty Sim.Account
      -> NEL.NonEmpty (NEL.NonEmpty (DynKeyPair,[SigCapability]))
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    buildGenAccountsKeysets accs cks =
      M.fromList . NEL.toList $ NEL.zipWith go accs cks

    go :: Sim.Account
       -> NEL.NonEmpty (DynKeyPair,[SigCapability])
       -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty (DynKeyPair,[SigCapability])))
    go name cks = (name, M.singleton (Sim.ContractName "coin") cks)

versionChains :: ChainwebVersion -> NESeq Sim.ChainId
versionChains = NES.fromList . NEL.fromList . chainIds

simpleExpressions
  :: Args
  -> ChainwebHost
  -> TVar Cut
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
simpleExpressions config (ChainwebHost h _p2p service) tcut tv distribution = do
  logg Info "Simple Expressions: Transactions are being generated"
  gencfg <- lift $ mkTXGConfig (Just distribution) config (HostAddress h service)

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let chs = maybe (versionChains $ nodeVersion config) NES.fromList
             . NEL.nonEmpty
             $ nodeChainIds config
      stt = TXGState gen tv chs

  evalStateT (runReaderT (runTXG (loop (confirmationDepth config) tcut generateSimpleTransactions)) gencfg) stt

pollRequestKeys :: Args -> ChainwebHost -> RequestKey -> IO ()
pollRequestKeys config (ChainwebHost h _p2p service) rkey = do
  cfg <- mkTXGConfig Nothing config (HostAddress h service)
  case confElasticSearchConfig cfg of
    Just _ -> putStrLn "Don't poll for request keys when using elasticsearch"
    Nothing -> do
      response <- pactPoll cfg cid (RequestKeys $ pure rkey)
      case response of
        Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
        Right (PollResponses a)
          | null a -> do
            putStrLn "Failure no result returned with error:"
            print a
            exitWith (ExitFailure 1)
          | otherwise -> print a >> exitSuccess
 where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: Sim.ChainId
    cid = fromMaybe 0 . listToMaybe $ nodeChainIds config

listenerRequestKey :: Args -> ChainwebHost -> ListenerRequest -> IO ()
listenerRequestKey config (ChainwebHost h _p2p service) (ListenerRequest rk) = do
  cfg <- mkTXGConfig Nothing config (HostAddress h service)
  pactListen cfg cid rk >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print r >> exitSuccess
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: Sim.ChainId
    cid = fromMaybe 0 . listToMaybe $ nodeChainIds config

-- | Send a single transaction to the network, and immediately listen for its result.
singleTransaction :: Args -> ChainwebHost -> SingleTX -> IO ()
singleTransaction args (ChainwebHost h _p2p service) (SingleTX c cid)
  | not . elem cid . chainIds $ nodeVersion args =
    putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
  | otherwise = do
      cfg <- mkTXGConfig Nothing args (HostAddress h service)
      kps <- testDynKeyPairs
      meta <- Sim.makeMeta cid (confTTL cfg) (confGasPrice cfg) (confGasLimit cfg)
      let v = confVersion cfg
      cmd <- mkExec c (datum kps) meta
        (NEL.toList kps)
        (Just $ CI.NetworkId $ chainwebVersionToText v)
        Nothing
      runExceptT (f cfg cmd) >>= \case
        Left e -> print e >> exitWith (ExitFailure 1)
        Right res -> pPrintNoColor res
  where
    datum :: NEL.NonEmpty (DynKeyPair,[SigCapability]) -> Value
    datum kps = object ["test-admin-keyset" .= fmap (formatPubKeyForCmd . fst) kps]

    f :: TXGConfig -> Command Text -> ExceptT ClientError IO ListenResponse
    f cfg cmd = do
      RequestKeys (rk NEL.:| _) <- ExceptT . pactSend cfg cid $ pure cmd
      ExceptT $ pactListen cfg cid rk


inMempool :: Args -> ChainwebHost -> (Sim.ChainId, [TransactionHash]) -> IO ()
inMempool args (ChainwebHost h _p2p service) (cid, txhashes)
    | not . elem cid . chainIds $ nodeVersion args =
      putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
    | otherwise = do
        cfg <- mkTXGConfig Nothing args (HostAddress h service)
        isMempoolMember cfg cid (TransactionHashes txhashes) >>= \case
          Left e -> print e >> exitWith (ExitFailure 1)
          Right res -> pPrintNoColor res


getInitialCut :: Manager -> Args -> IO Cut
getInitialCut mgr cfg = do
  let policy :: RetryPolicyM IO
      policy = exponentialBackoff 250_000 <> limitRetries 3
      toRetry _ = \case
        Right _ -> return False
        Left (ApiError RateLimiting _ _) -> return True
        Left (ApiError EClientError _ _) -> return False
        Left (ApiError ServerError _ _) -> return True
        Left (ApiError (OtherError _) _ _) -> return False
      p2pHost (ChainwebHost host p2p _) = HostAddress host p2p
  retrying policy toRetry (const $ queryCut mgr (p2pHost $ head $ chainwebHosts cfg) (nodeVersion cfg)) >>= \case
    Right cut -> pure cut
    Left err -> die $ "cut processing: " <> show err

cutLoop :: Manager -> TVar Cut -> ChainwebHost -> ChainwebVersion -> IO ()
cutLoop mgr tcut addr version = forever $ do
  threadDelay $ 60 * 1000_000
  let policy = exponentialBackoff 250_000 <> limitRetries 3
      toRetry _ = \case
        Right _ -> return False
        Left (ApiError RateLimiting _ _) -> return True
        Left (ApiError EClientError _ _) -> return False
        Left (ApiError ServerError _ _) -> return True
        Left (ApiError (OtherError _) _ _) -> return False
      p2pHost (ChainwebHost host p2p _) = HostAddress host p2p
  cut <- retrying policy toRetry (const $ queryCut mgr (p2pHost addr) version) >>= \case
    Left err -> throwIO $ userError $ show err
    Right cut -> pure cut

  atomically $
  -- assumption --
  -- given a delay of 60 seconds, let's assume that the cut has changed
  -- assumption --
    writeTVar tcut cut


work :: Args -> IO ()
work cfg = do
  tv  <- newTVarIO 0
  let nds = zipWith toNodeData [0..] (chainwebHosts cfg)
  _trkeys :: TVar PollMap <- newTVarIO $ M.fromList $ zip nds (repeat $ M.fromList $ zip (nodeChainIds cfg) (repeat mempty))
  mgr <- unsafeManager
  cut <- getInitialCut mgr cfg
  tcut <- newTVarIO cut
  let startCutLoop =
        forkFinally (cutLoop mgr tcut (head $ chainwebHosts cfg) (nodeVersion cfg)) $ either throwIO pure
  withLog $ \l -> forConcurrently_ (chainwebHosts cfg) $ \chainwebHost ->
    runLoggerT (act startCutLoop tcut tv chainwebHost) l
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ logLevel cfg
    withLog inner = Y.withHandleBackend_ id (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

    act :: IO ThreadId -> TVar Cut -> TVar TXCount -> ChainwebHost -> LoggerT T.Text IO ()
    act startCutLoop tcut tv chainwebHost@(ChainwebHost h _p2p service) = localScope (const [(hostnameToText h, portToText service)]) $
      case scriptCommand cfg of
        DeployContracts [] -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg chainwebHost $ NEL.cons (initAdminKeysetContract v) (defaultContractLoaders v)
        DeployContracts cs -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg chainwebHost $ initAdminKeysetContract v NEL.:| map (createLoader v) cs
        RunStandardContracts distribution -> do
          void $ liftIO $ startCutLoop
          realTransactions cfg chainwebHost tcut tv distribution
        RunCoinContract distribution -> do
          void $ liftIO $ startCutLoop
          realCoinTransactions cfg chainwebHost tcut tv distribution
        RunXChainTransfer _distribution -> error "xchain transfers not yet implemented"
          -- _realXChainCoinTransactions cfg chainwebHost tv distribution
        RunSimpleExpressions distribution -> do
          void $ liftIO $ startCutLoop
          simpleExpressions cfg chainwebHost tcut tv distribution
        PollRequestKeys rk -> liftIO $ pollRequestKeys cfg chainwebHost
          . RequestKey
          . H.Hash
          . SB.toShort
          . T.encodeUtf8
          $ rk
        ListenerRequestKey rk -> liftIO $ listenerRequestKey cfg chainwebHost
          . ListenerRequest
          . RequestKey
          . H.Hash
          . SB.toShort
          . T.encodeUtf8
          $ rk
        SingleTransaction stx -> liftIO $
          singleTransaction cfg chainwebHost stx
        MempoolMember req -> liftIO $ inMempool cfg chainwebHost req


main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = chainIds $ nodeVersion config
      isMem  = all (`elem` chains) $ nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ nodeChainIds config)
  pPrintNoColor config
  work config

mainInfo :: ProgramInfo Args
mainInfo =
  programInfo
    "Chainweb-TransactionGenerator"
    scriptConfigParser
    defaultArgs

-- TODO: This is here for when a user wishes to deploy their own
-- contract to chainweb. We will have to carefully consider which
-- chain we'd like to send the contract to.

-- TODO: This function should also incorporate a user's keyset as well
-- if it is given.
createLoader :: ChainwebVersion -> Sim.ContractName -> ContractLoader
createLoader v (Sim.ContractName contractName) meta kp = do
  theCode <- readFile (contractName <> ".pact")
  adminKS <- testDynKeyPairs
  -- TODO: theData may change later
  let theData = object [ (fromText "admin-keyset") .= fmap (formatPubKeyForCmd . fst) adminKS, (fromText $ T.append (T.pack contractName) "-keyset") .= fmap (formatPubKeyForCmd . fst) kp ]

  mkExec (T.pack theCode) theData meta (NEL.toList adminKS) (Just $ CI.NetworkId $ chainwebVersionToText v) Nothing

-- Remember that coin contract is already loaded.
defaultContractLoaders :: ChainwebVersion -> NEL.NonEmpty ContractLoader
defaultContractLoaders v =
    NEL.fromList [ helloWorldContractLoader v, simplePaymentsContractLoader v]

---------------------------
-- FOR DEBUGGING IN GHCI --
---------------------------
{-
_genapi3 :: ChainwebVersion -> ChainId -> Text
_genapi3 version chainid =
  case someChainwebVersionVal version of
    SomeChainwebVersionT (_ :: Proxy cv) ->
      case someChainIdVal chainid of
        SomeChainIdT (_ :: Proxy cid) ->
          -- client (Proxy :: Proxy (MempoolApi cv cid))
          let p = (Proxy :: Proxy (MempoolMemberApi cv cid))
          in toUrlPiece $ safeLink (Proxy :: (Proxy (MempoolApi cv cid))) p
-}
