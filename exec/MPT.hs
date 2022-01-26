{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- | Module: MempoolP2PTester
-- Copyright: Copyright © 2018 - 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Main where

import           Network.HTTP.Client hiding (host)
import           Network.HTTP.Types

import           Configuration.Utils hiding (Error, Lens')
import           Control.Concurrent
import           Control.Concurrent.Async hiding (poll)
import           Control.Concurrent.STM
import           Control.Exception (catches, displayException, throwIO, Handler(..), SomeException(..))
import           Control.Lens hiding (op, (.=), (|>))
import           Control.Monad.Except
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import           Control.Retry
import qualified Data.Aeson.Types as A
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LB
import           Data.Generics.Product.Fields (field)
import           Data.Foldable
import           Data.Functor.Compose
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.IORef
import           Data.List (delete)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL ()
import           Fake (fake, generate)
import           Network.HostAddress
import           Pact.Types.API
import           Pact.Types.Capability
import qualified Pact.Types.ChainMeta as CM
import           Pact.Types.Command
import           Pact.Types.Exp (Literal(..))
import           Pact.Types.Gas
import           Pact.Types.Info (mkInfo)
import           Pact.Types.Names
import           Pact.Types.PactValue
import           Pact.Types.PactError
import           System.Exit (die)
import           System.Logger hiding (StdOut)
import qualified System.Logger as Y
import           System.Random.MWC (createSystemRandom, uniformR)
import           System.Random.MWC.Distributions (normal)
import           Text.Pretty.Simple (pPrintNoColor)
import           Text.Printf
import           TXG.Simulate.Contracts.CoinContract
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Simulate.Contracts.HelloWorld
import           TXG.Simulate.Contracts.SimplePayments
import           TXG.Types
import           TXG.Utils
import           MPT.Types

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = chainIds (mpt_nodeVersion config)
      isMem = all (`elem` chains) $ mpt_nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ mpt_nodeChainIds config)
  pPrintNoColor config
  worker config

mainInfo :: ProgramInfo MPTArgs
mainInfo =
  programInfo
  "MempoolP2PTester"
  mpt_scriptConfigParser
  defaultMPTArgs

worker :: MPTArgs -> IO ()
worker config = do
    tv  <- newTVarIO 0
    let nds = zipWith toNodeData [0..] (mpt_hosts config)
    trkeys <- newTVarIO $ M.fromList $ zip nds (repeat $ M.fromList $ zip (mpt_nodeChainIds config) (repeat mempty))
    mgr <- unsafeManager
    createNodeTable (T.unpack $ mpt_dbFile config)
    mapM_ (registerNode $ mpt_dbFile config) nds
    createMempoolStatTable (T.unpack $ mpt_dbFile config)
    let policy :: RetryPolicyM IO
        policy = exponentialBackoff 250_000 <> limitRetries 3
        toRetry _ = \case
          Right _ -> return False
          Left (ApiError RateLimiting _ _) -> return True
          Left (ApiError MPT.Types.ClientError _ _) -> return False
          Left (ApiError ServerError _ _) -> return True
          Left (ApiError (OtherError _) _ _) -> return False
    cut <- retrying policy toRetry (const $ queryCut mgr (head $ mpt_hosts config) (mpt_nodeVersion config)) >>= \case
        Right cut -> pure cut
        Left err -> die $ "cut processing: " <> show err
    tcut <- newTVarIO cut
    _ <- forkFinally (cutLoop mgr tcut (head $ mpt_hosts config) (mpt_nodeVersion config)) $ either throwIO pure
    hostkey <- newIORef 0
    withLog $ \l -> forConcurrently_ (mpt_hosts config) $ \host -> do
      let cfg = TXGConfig { confTimingDist = Just $ UniformTD (Uniform (fromIntegral $ mpt_transferRate config) (fromIntegral $ mpt_transferRate config))
          , confKeysets = mempty
          , confHost = HostAddress (cwh_hostname host) (cwh_servicePort host)
          , confManager = mgr
          , confVersion = mpt_nodeVersion config
          , confBatchSize = mpt_batchSize config
          , confVerbose = mpt_verbose config
          , confGasLimit = mpt_gasLimit config
          , confGasPrice = mpt_gasPrice config
          , confTTL = mpt_timetolive config
          }
      cids <- newIORef $ NES.fromList $ NEL.fromList $ mpt_nodeChainIds config
      _ <- liftIO $ forkFinally (pollLoop cids (mpt_confirmationDepth config) (mpt_dbFile config) (mpt_pollDelay config) tcut trkeys cfg) $ either throwIO pure
      _key <- readIORef hostkey
      atomicModifyIORef' hostkey (\i -> (i+1, ()))
      runLoggerT (act _key tv tcut trkeys cfg) l
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ Info
    withLog inner = Y.withHandleBackend_ id (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner
    act :: Integer -> TVar TXCount -> TVar Cut -> TVar PollMap -> TXGConfig -> LoggerT T.Text IO ()
    act k tv tcut trkeys cfg = localScope (const [(hostnameToText (_hostAddressHost $ confHost cfg), portToText (_hostAddressPort $ confHost cfg))]) $ do
      coinTransfers k config tv tcut trkeys cfg


cutLoop :: Manager -> TVar Cut -> ChainwebHost -> ChainwebVersion -> IO ()
cutLoop mgr tcut addr version = forever $ do
  threadDelay $ 60 * 1000000
  let policy = exponentialBackoff 250_000 <> limitRetries 3
      toRetry _ = \case
        Right _ -> return False
        Left (ApiError RateLimiting _ _) -> return True
        Left (ApiError MPT.Types.ClientError _ _) -> return False
        Left (ApiError ServerError _ _) -> return True
        Left (ApiError (OtherError _) _ _) -> return False
  ecut <- retrying policy toRetry (const $ queryCut mgr addr version)
  cut <- case ecut of
    Left err -> throwIO $ userError $ show err
    Right cut -> pure cut
  atomically $
    -- ASSUMPTION --
    -- Given a delay of 60 seconds, let's assume that the cut has changed.
    -- ASSUMPTION --
    writeTVar tcut cut

inRequestKeys :: RequestKey -> RequestKeys -> Bool
inRequestKeys rk (RequestKeys rks) = elem rk rks

deleteRequestKey :: ChainId -> RequestKey -> NodeData -> PollMap -> PollMap
deleteRequestKey cid rk nd m = M.adjust (M.adjust (mapMaybe go) cid) nd m
  where
    go (ts, RequestKeys rks) =
      case delete rk (NEL.toList rks) of
        [] -> Nothing
        rkss -> Just (ts, RequestKeys $ NEL.fromList rkss)

pollLoop :: IORef (NESeq ChainId) -> Int -> T.Text -> Int -> TVar Cut -> TVar PollMap -> TXGConfig -> IO ()
pollLoop cids confirmationDepth dbFile secondsDelay tcut trkeys config = forever $ do
  threadDelay $ secondsDelay * 1000000
  m <- readTVarIO trkeys
  cid <- readIORef cids >>= pure . NES.head
  modifyIORef cids rotate
  forM_ (M.toList m) $ \(nd, m') -> do
    putStrLn $ "current node: " <> show (nodeData_name nd)
    case M.lookup cid m' of
      Nothing -> return ()
      Just res -> do
        let rkeys = RequestKeys $ foldr1 (<>) $ map (\(_, RequestKeys rks) -> rks) res
        pollRequestKeys config cid rkeys >>= \resp -> do
          case resp of
            Left err -> printf "Caught this error while polling for these request keys (%s) %s\n" (show rkeys) err
            Right (poll_start,poll_end,result) -> iforM_ result $ \rk json_res -> do
                case find (inRequestKeys rk . snd) res of
                  Nothing -> throwIO $ userError "couldn't find time span for request key"
                  Just (ts,_) -> do
                    atomically $ modifyTVar trkeys (deleteRequestKey cid rk nd)
                    transmitMempoolStat dbFile $ MempoolStat
                      {
                        ms_chainid = cid
                      , ms_txhash = rk
                      , ms_stat = TimeUntilMempoolAcceptance ts
                      , ms_nodekey = nodeData_key nd
                      }
                    transmitMempoolStat dbFile $ MempoolStat
                      {
                        ms_chainid = cid
                      , ms_txhash = rk
                      , ms_stat = TimeUntilBlockInclusion (TimeSpan poll_start poll_end)
                      , ms_nodekey = nodeData_key nd
                      }
                    let h = fromIntegral $ fromJuste $ json_res ^? _2 . key "blockHeight" . _Integer
                    cstart <- getCurrentTimeInt64
                    withAsync (loopUntilConfirmationDepth confirmationDepth cid h tcut) $ \a' -> do
                      cend <- wait a'
                      transmitMempoolStat dbFile $ MempoolStat
                        {
                            ms_chainid = cid
                        ,  ms_txhash = rk
                        , ms_stat = TimeUntilConfirmationDepth (TimeSpan cstart cend)
                        , ms_nodekey = nodeData_key nd
                      }

mkMPTConfig
  :: Maybe TimingDistribution
  -> Manager
  -> MPTArgs
  -> HostAddress
  -> TXGConfig
mkMPTConfig mdistribution manager mpt_config hostAddr =
  TXGConfig
    {
      confTimingDist = mdistribution
    , confKeysets = mempty
    , confHost = hostAddr
    , confManager = manager
    , confVersion = mpt_nodeVersion mpt_config
    , confBatchSize = mpt_batchSize mpt_config
    , confVerbose = mpt_verbose mpt_config
    , confGasLimit = mpt_gasLimit mpt_config
    , confGasPrice = mpt_gasPrice mpt_config
    , confTTL = mpt_timetolive mpt_config
    }

data ApiError = ApiError
  { apiError_type :: ErrorType
  , apiError_status :: Status
  , apiError_body :: LB.ByteString
  } deriving (Eq,Ord,Show)

handleRequest :: Request -> Manager -> IO (Either ApiError (Response LB.ByteString))
handleRequest req mgr = do
  res <- httpLbs req mgr
  let mkErr t = ApiError t (responseStatus res) (responseBody res)
      checkErr s
        | statusCode s == 429 || statusCode s == 403 = Left $ mkErr RateLimiting
        | statusIsClientError s = Left $ mkErr MPT.Types.ClientError
        | statusIsServerError s = Left $ mkErr ServerError
        | statusIsSuccessful s = Right res
        | otherwise = Left $ mkErr $ OtherError "unknown error"
  pure $ checkErr (responseStatus res)

type BlockHeight = Int

data NodeInfo = NodeInfo
  { _nodeInfo_chainwebVer :: Text
  , _nodeInfo_apiVer      :: Text
  , _nodeInfo_chains      :: Set ChainId
  , _nodeInfo_numChains   :: Int
  , _nodeInfo_graphs      :: Maybe [(BlockHeight, [(Int, [Int])])]
  } deriving (Eq,Ord,Show)

instance FromJSON NodeInfo where
  parseJSON = withObject "NodeInfo" $ \o -> NodeInfo
      <$> o .: "nodeVersion"
      <*> o .: "nodeApiVersion"
      <*> f (o .: "nodeChains")
      <*> o .: "nodeNumberOfChains"
      <*> o .:? "nodeGraphHistory"
    where
      f :: A.Parser (Set Text) -> A.Parser (Set ChainId)
      f as = do
        s <- as
        let ecids = traverse cidFromText (S.toList s)
        case ecids of
          Left ex -> fail (displayException ex)
          Right cids -> return (S.fromList cids)

queryCut :: Manager -> ChainwebHost -> ChainwebVersion -> IO (Either ApiError Cut)
queryCut mgr (ChainwebHost h p2p _service) version = do
  let url = "https://" <> hostnameToText h <> ":" <> portToText p2p <> "/chainweb/0.0/" <> chainwebVersionToText version <> "/cut"
  req <- parseRequest $ T.unpack url
  res <- handleRequest req mgr
  pure $ responseBody <$> res

loopUntilConfirmationDepth :: Int -> ChainId -> BlockHeight -> TVar Cut -> IO Int64
loopUntilConfirmationDepth confirmationDepth cid startHeight tcut = do
  atomically $ do
    cut <- readTVar tcut
    let height = fromIntegral $ fromJuste $ cut ^? key "hashes" . key (cidToText cid) . key "height" . _Integer
    check (height >= (startHeight + confirmationDepth))
  getCurrentTimeInt64

loop
  :: (MonadIO m, MonadLog T.Text m)
  => Integer
  -> TXG MPTState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
  -> TXG MPTState m ()
loop k f = forever $ do
  liftIO $ threadDelay $ 10_000_000
  (cid, msgs, transactions) <- f
  config <- ask
  lift $ logg Info $ "The number of transactions we are sending " <> T.pack (show (length transactions))
  (requestKeys, start, end) <- liftIO $ trackTime $ pactSend config cid transactions

  case requestKeys of
    Left servantError ->
      lift . logg Error $ T.pack (show servantError)
    Right rks -> do
      countTV <- gets mptCounter
      trkeys <- gets mptPollMap
      batch <- asks confBatchSize
      liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
      count <- liftIO $ readTVarIO countTV
      lift . logg Info $ "Transaction count: " <> T.pack (show count)
      lift . logg Info $ "Transaction requestKey: " <> T.pack (show rks)
      let retrier = retrying policy (const (pure . isRight)) . const
          policy :: RetryPolicyM IO
          policy = exponentialBackoff 50_000 <> limitRetries 10
      poll_result <- liftIO $ retrier $ pollRequestKeys config cid rks
      case poll_result of
        Left err -> lift $ logg Error $ T.pack $ printf "Caught this error while polling for these request keys (%s) %s" (show rks) err
        Right (poll_start, poll_end, result) -> liftIO $ iforM_ result $ \rk res -> do
            transmitMempoolStat dbFile $ MempoolStat
              {
                ms_chainid = cid
              , ms_txhash = rk
              , ms_stat = TimeUntilMempoolAcceptance (TimeSpan start end)
              }
            transmitMempoolStat dbFile $ MempoolStat
              {
                ms_chainid = cid
              , ms_txhash = rk
              , ms_stat = TimeUntilBlockInclusion (TimeSpan poll_start poll_end)
              }
            let h = fromIntegral $ fromJuste $ res ^? _2 . key "blockHeight" . _Integer
            cstart <- getCurrentTimeInt64
            withAsync (loopUntilConfirmationDepth confirmationDepth cid h tcut) $ \a' -> do
              cend <- wait a'
              transmitMempoolStat dbFile $ MempoolStat
                {
                    ms_chainid = cid
                ,  ms_txhash = rk
                , ms_stat = TimeUntilConfirmationDepth (TimeSpan cstart cend)
                }

      forM_ (Compose msgs) $ \m ->
        lift . logg Info $ "Actual transaction: " <> m

versionChains :: ChainwebVersion -> NESeq Sim.ChainId
versionChains = NES.fromList . NEL.fromList . chainIds

generateDelay :: MonadIO m => TXG MPTState m Int
generateDelay = do
  distribution <- asks confTimingDist
  gen <- gets mptGen
  case distribution of
    Just (GaussianTD (Gaussian gmean gvar)) -> liftIO (truncate <$> normal gmean gvar gen)
    Just (UniformTD (Uniform ulow uhigh)) -> liftIO (truncate <$> uniformR (ulow, uhigh) gen)
    Nothing -> error "generateDelay: impossible"

-- | O(1). The head value is moved to the end.
rotate :: NESeq a -> NESeq a
rotate (h :<|| rest) = rest :||> h

data CmdChoice = CoinContract | HelloWorld | Payments
  deriving (Show, Eq, Ord, Bounded, Enum)

generateTransactions
    :: forall m. (MonadIO m, MonadLog T.Text m)
    => Bool
    -> Verbose
    -> CmdChoice
    -> TXG MPTState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
generateTransactions ifCoinOnlyTransfers isVerbose contractIndex = do
  -- Choose a Chain to send this transaction to, and cycle the state.
  cid <- NES.head <$> gets mptChains
  field @"mptChains" %= rotate

  cks <- asks confKeysets
  version <- asks confVersion
  case M.lookup cid cks of
    Nothing -> error $ printf "%s is missing Accounts!" (show cid)
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
        -> Map Sim.Account (NEL.NonEmpty SomeKeyPairCaps)
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

    mkTransferCaps :: ReceiverName -> Sim.Amount -> (Sim.Account, NEL.NonEmpty SomeKeyPairCaps) -> (Sim.Account, NEL.NonEmpty SomeKeyPairCaps)
    mkTransferCaps (ReceiverName (Sim.Account r)) (Sim.Amount m) (s@(Sim.Account ss),ks) = (s, (caps <$) <$> ks)
      where caps = [gas,tfr]
            gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
            tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                  [ PLiteral $ LString $ T.pack ss
                  , PLiteral $ LString $ T.pack r
                  , PLiteral $ LDecimal m]

    payments :: GasLimit -> GasPrice -> CM.TTLSeconds -> ChainwebVersion -> Sim.ChainId -> Map Sim.Account (NEL.NonEmpty SomeKeyPairCaps) -> IO (Command Text)
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

coinTransfers
  :: Integer
   -> MPTArgs
   -> TVar TXCount
   -> TVar Cut
   -> TVar PollMap
   -> TXGConfig
   -> LoggerT T.Text IO ()
coinTransfers k config tv tcut trkeys cfg = do

    let chains = maybe (versionChains (mpt_nodeVersion config)) NES.fromList
                  . NEL.nonEmpty
                  $ mpt_nodeChainIds config

    accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
      let f (Sim.Account sender) = do
            !meta <- liftIO (Sim.makeMetaWithSender sender (confTTL cfg) (confGasPrice cfg) (confGasLimit cfg) cid)
            Sim.createCoinAccount (confVersion cfg) meta sender
      (coinKS, _coinAcc) <-
        liftIO $ unzip <$> traverse f Sim.coinAccountNames
      let accounts = buildGenAccountsKeysets Sim.coinAccountNames coinKS
      pure (cid, accounts)

    logg Info "Real Transactions: Transactions are being generated"

    -- set up values for running the effect stack?
    gen <- liftIO createSystemRandom
    let act = loop k (generateTransactions True (mpt_verbose config) CoinContract)
        env = set (field @"confKeysets") accountMap cfg
          & over (field @"confKeysets") (fmap (M.filterWithKey (\(Sim.Account _k) _ -> _k `elem` (mpt_accounts config))))
        stt = MPTState gen tv tcut trkeys chains

    evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: [Sim.Account]
      -> [NEL.NonEmpty SomeKeyPairCaps]
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    buildGenAccountsKeysets accs cks =
      M.fromList $ zipWith go accs cks

    go :: Sim.Account
      -> NEL.NonEmpty SomeKeyPairCaps
      -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    go name cks = (name, M.singleton (Sim.ContractName "coin") cks)

pactSend
  :: Foldable f
  => TXGConfig
  -> Sim.ChainId
  -> f (Command Text)
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

pollRequestKeys :: TXGConfig -> ChainId -> RequestKeys -> IO (Either String (Int64, Int64, (HM.HashMap RequestKey (Maybe PactError, Value))))
pollRequestKeys cfg cid rkeys = do
    (response, start, end) <- trackTime $ pactPoll cfg cid rkeys
    case response of
      Left _ -> pure $ Left "Failure"
      Right (PollResponses as)
        | null as -> pure $ Left "Failure no result returned"
        | otherwise -> pure $ Right $ (start, end, fmap f as)
  where
    f cr = (either Just (const Nothing) $ _pactResult $ _crResult cr, fromJuste $ _crMetaData cr)


{-
PARAMETERS THAT WE NEED TO ACCEPT

1. rate of transfers
2. gas price
3. sender accounts (not sure if it would make a difference if we only use a single account for all transfers)
4. receiver accounts (again, maybe a single account would be fine)
5. batch size
6. confirmation depth

-}


{-

THIS SCRIPT NEEDS TO TRACK THESE TIMES

(done) 1. the time that it takes until txs is accepted in the mempool (send returns requestkey)
(done) 2. the time that it takes until txs is included in a block
(done) 3. the time that it takes until txs reach confirmation depth

-}

data MempoolStat = MempoolStat
  {
    ms_chainid :: !ChainId
  , ms_txhash :: !RequestKey
  , ms_stat :: !MempoolStat'
  , ms_nodekey :: !Integer
  } deriving Show

data MempoolStat' =
  TimeUntilMempoolAcceptance TimeSpan
  | TimeUntilBlockInclusion TimeSpan
  | TimeUntilConfirmationDepth TimeSpan
  deriving Show

-- This is the current number of microseconds since unix epoch.
getCurrentTimeInt64 :: IO Int64
getCurrentTimeInt64 = do
    -- returns POSIX seconds with picosecond precision
    t <- getPOSIXTime
    return $! round $ t * 1000_000

trackTime :: IO a -> IO (a, Int64, Int64)
trackTime act = do
  t1 <- getCurrentTimeInt64
  r <- act
  t2 <- getCurrentTimeInt64
  return (r,t1,t2)

instance SQL.ToRow MempoolStat where
  toRow (MempoolStat (ChainId cid) (RequestKey rk) ms nodekey) = SQL.toRow (show rk, cid, ty, s, e, nodekey)
    where
      (ty,s,e) =
        case ms of
          TimeUntilMempoolAcceptance (TimeSpan s' e') -> ("mempool-inclusion" :: Text, s', e')
          TimeUntilBlockInclusion (TimeSpan s' e')    -> ("block-inclusion", s', e')
          TimeUntilConfirmationDepth (TimeSpan s' e') -> ("depth-confirmation", s', e')

registerNode :: Text -> NodeData -> IO ()
registerNode dbFile nd = do
    SQL.withConnection (T.unpack dbFile) $ \conn -> do
      putStrLn $ "writing node data: " <> show nd
      SQL.execute conn insertStmt nd
        `catches`
        [
          Handler $ \err@(SQL.SQLError sqlerr details context) -> case sqlerr of
            SQL.ErrorConstraint -> do
              putStrLn "Caught a constraint violation"
              printf "Error context: %s\n" context
              printf "Error details: %s\n" details
            _ -> throwIO err
        , Handler $ \e@(SomeException _) -> throwIO e
        ]
  where
    insertStmt =
      "INSERT INTO node (key,name) VALUES (?,?)"

transmitMempoolStat :: Text -> MempoolStat -> IO ()
transmitMempoolStat dbFile ms = do
    SQL.withConnection (T.unpack dbFile) $ \conn -> do
      putStrLn $ "writing mempool stat: " <> show ms
      SQL.execute conn insertStmt ms
        `catches`
        [
          Handler $ \err@(SQL.SQLError sqlerr details context) -> case sqlerr of
            SQL.ErrorConstraint -> do
              putStrLn "Caught a constraint violation"
              printf "Error context: %s\n" context
              printf "Error details: %s\n" details
            _ -> throwIO err
        , Handler $ \e@(SomeException _) -> throwIO e
        ]
  where
    insertStmt =
      "INSERT INTO mpt_stat (requestkey, chainid, stat_type, start_time, end_time, node_key) VALUES (?,?,?,?,?,?)"

createNodeTable :: FilePath -> IO ()
createNodeTable dbFile = do
  SQL.withConnection dbFile $ \conn ->
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS node (key INTEGER NOT NULL, name VARCHAR, PRIMARY KEY (key))"

createMempoolStatTable :: FilePath -> IO ()
createMempoolStatTable dbFile =
  SQL.withConnection dbFile $ \conn ->
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS mpt_stat \
                  \( requestkey VARCHAR NOT NULL\
                  \, chainid INTEGER NOT NULL\
                  \, stat_type VARCHAR NOT NULL\
                  \, start_time INTEGER NOT NULL\
                  \, end_time INTEGER NOT NULL\
                  \, node_key INTEGER NOT NULL)"

