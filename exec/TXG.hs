{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
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

import           Configuration.Utils hiding (Error, Lens')
import           Control.Concurrent
import           Control.Concurrent.Async hiding (poll)
import           Control.Concurrent.STM
import           Control.Lens hiding (op, (.=), (|>))
import           Control.Monad.Except
import           Control.Monad.Reader hiding (local)
import           Control.Monad.State.Strict
import           Data.Generics.Product.Fields (field)
import           Data.Foldable
import           Data.Functor.Compose
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Sequence.NonEmpty (NESeq(..))
import qualified Data.Sequence.NonEmpty as NES
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Fake (fake, generate)
import           Network.HostAddress
import           Pact.ApiReq
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
import           Pact.Types.PactValue
import           System.Logger hiding (StdOut)
import qualified System.Logger as Y
import           System.Exit
import           System.Random
import           System.Random.MWC (createSystemRandom, uniformR)
import           System.Random.MWC.Distributions (normal)
import           Text.Pretty.Simple (pPrintNoColor)
import           Text.Printf
import           TXG.Simulate.Contracts.CoinContract
import qualified TXG.Simulate.Contracts.Common as Sim
import           TXG.Simulate.Contracts.HelloWorld
import           TXG.Simulate.Contracts.SimplePayments
import           TXG.Simulate.Utils
import           TXG.Utils
import           TXG.Types

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
      kps <- testSomeKeyPairs

      let theData = object ["test-admin-keyset" .= fmap (formatB16PubKey . fst) kps]
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

isMempoolMember
  :: TXGConfig
  -> Sim.ChainId
  -> [TransactionHash]
  -> IO (Either ClientError [Bool])
isMempoolMember c cid = mempoolMember
  (confManager c)
  (confHost c)
  (confVersion c)
  cid

loop
  :: (MonadIO m, MonadLog T.Text m)
  => TXG TXGState m (Sim.ChainId, NEL.NonEmpty (Maybe Text), NEL.NonEmpty (Command Text))
  -> TXG TXGState m ()
loop f = do
  (cid, msgs, transactions) <- f
  config <- ask
  requestKeys <- liftIO $ pactSend config cid transactions

  case requestKeys of
    Left servantError ->
      lift . logg Error $ T.pack (show servantError)
    Right rk -> do
      countTV <- gets gsCounter
      batch <- asks confBatchSize
      liftIO . atomically $ modifyTVar' countTV (+ fromIntegral batch)
      count <- liftIO $ readTVarIO countTV
      lift . logg Info $ "Transaction count: " <> T.pack (show count)
      lift . logg Info $ "Transaction requestKey: " <> T.pack (show rk)
      forM_ (Compose msgs) $ \m ->
        lift . logg Info $ "Actual transaction: " <> m

  loop f

type ContractLoader
    = CM.PublicMeta -> NEL.NonEmpty SomeKeyPairCaps -> IO (Command Text)

loadContracts :: Args -> HostAddress -> NEL.NonEmpty ContractLoader -> IO ()
loadContracts config host contractLoaders = do
  conf@(TXGConfig _ _ _ _ _ _ (Verbose vb) tgasLimit tgasPrice ttl') <- mkTXGConfig Nothing config host
  forM_ (nodeChainIds config) $ \cid -> do
    !meta <- Sim.makeMeta cid ttl' tgasPrice tgasLimit
    ts <- testSomeKeyPairs
    contracts <- traverse (\f -> f meta ts) contractLoaders
    pollresponse <- runExceptT $ do
      rkeys <- ExceptT $ pactSend conf cid contracts
      when vb
        $ withConsoleLogger Info
        $ logg Info
        $ "sent contracts with request key: " <> T.pack (show rkeys)
      ExceptT $ pactPoll conf cid rkeys
    withConsoleLogger Info . logg Info $ T.pack (show pollresponse)

realTransactions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
realTransactions config host tv distribution = do
  cfg@(TXGConfig _ _ _ _ v _ _ tgasLimit tgasPrice ttl') <- liftIO $ mkTXGConfig (Just distribution) config host

  let chains = maybe (versionChains $ nodeVersion config) NES.fromList
               . NEL.nonEmpty
               $ nodeChainIds config

  accountMap <- fmap (M.fromList . toList) . forM chains $ \cid -> do
    !meta <- liftIO $ Sim.makeMeta cid ttl' tgasPrice tgasLimit
    (paymentKS, paymentAcc) <- liftIO $ NEL.unzip <$> Sim.createPaymentsAccounts v meta
    (coinKS, coinAcc) <- liftIO $ NEL.unzip <$> Sim.createCoinAccounts v meta
    pollresponse <- liftIO . runExceptT $ do
      rkeys <- ExceptT $ pactSend cfg cid (paymentAcc <> coinAcc)
      ExceptT $ pactPoll cfg cid rkeys
    case pollresponse of
      Left e  -> logg Error $ T.pack (show e)
      Right _ -> pure ()
    let accounts = buildGenAccountsKeysets Sim.accountNames paymentKS coinKS
    pure (cid, accounts)

  logg Info "Real Transactions: Transactions are being generated"

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let act = loop (liftIO randomEnum >>= generateTransactions False (verbose config))
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NEL.NonEmpty Sim.Account
      -> NEL.NonEmpty (NEL.NonEmpty SomeKeyPairCaps)
      -> NEL.NonEmpty (NEL.NonEmpty SomeKeyPairCaps)
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    buildGenAccountsKeysets accs pks cks =
      M.fromList . NEL.toList $ nelZipWith3 go accs pks cks

    go :: Sim.Account
       -> NEL.NonEmpty SomeKeyPairCaps
       -> NEL.NonEmpty SomeKeyPairCaps
       -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    go name pks cks = (name, M.fromList [ps, cs])
      where
        ps = (Sim.ContractName "payment", pks)
        cs = (Sim.ContractName "coin", cks)

realCoinTransactions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
realCoinTransactions config host tv distribution = do
  cfg <- liftIO $ mkTXGConfig (Just distribution) config host

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
  let act = loop (generateTransactions True (verbose config) CoinContract)
      env = set (field @"confKeysets") accountMap cfg
      stt = TXGState gen tv chains

  evalStateT (runReaderT (runTXG act) env) stt
  where
    buildGenAccountsKeysets
      :: NEL.NonEmpty Sim.Account
      -> NEL.NonEmpty (NEL.NonEmpty SomeKeyPairCaps)
      -> Map Sim.Account (Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    buildGenAccountsKeysets accs cks =
      M.fromList . NEL.toList $ NEL.zipWith go accs cks

    go :: Sim.Account
       -> NEL.NonEmpty SomeKeyPairCaps
       -> (Sim.Account, Map Sim.ContractName (NEL.NonEmpty SomeKeyPairCaps))
    go name cks = (name, M.singleton (Sim.ContractName "coin") cks)

versionChains :: ChainwebVersion -> NESeq Sim.ChainId
versionChains = NES.fromList . NEL.fromList . chainIds

simpleExpressions
  :: Args
  -> HostAddress
  -> TVar TXCount
  -> TimingDistribution
  -> LoggerT T.Text IO ()
simpleExpressions config host tv distribution = do
  logg Info "Simple Expressions: Transactions are being generated"
  gencfg <- lift $ mkTXGConfig (Just distribution) config host

  -- Set up values for running the effect stack.
  gen <- liftIO createSystemRandom
  let chs = maybe (versionChains $ nodeVersion config) NES.fromList
             . NEL.nonEmpty
             $ nodeChainIds config
      stt = TXGState gen tv chs

  evalStateT (runReaderT (runTXG (loop generateSimpleTransactions)) gencfg) stt

pollRequestKeys :: Args -> HostAddress -> RequestKey -> IO ()
pollRequestKeys config host rkey = do
  cfg <- mkTXGConfig Nothing config host
  response <- pactPoll cfg cid (RequestKeys $ pure rkey)
  case response of
    Left _ -> putStrLn "Failure" >> exitWith (ExitFailure 1)
    Right (PollResponses a)
      | null a -> putStrLn "Failure no result returned" >> exitWith (ExitFailure 1)
      | otherwise -> print a >> exitSuccess
 where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: Sim.ChainId
    cid = fromMaybe 0 . listToMaybe $ nodeChainIds config

listenerRequestKey :: Args -> HostAddress -> ListenerRequest -> IO ()
listenerRequestKey config host (ListenerRequest rk) = do
  cfg <- mkTXGConfig Nothing config host
  pactListen cfg cid rk >>= \case
    Left err -> print err >> exitWith (ExitFailure 1)
    Right r -> print r >> exitSuccess
  where
    -- | It is assumed that the user has passed in a single, specific Chain that
    -- they wish to query.
    cid :: Sim.ChainId
    cid = fromMaybe 0 . listToMaybe $ nodeChainIds config

-- | Send a single transaction to the network, and immediately listen for its result.
singleTransaction :: Args -> HostAddress -> SingleTX -> IO ()
singleTransaction args host (SingleTX c cid)
  | not . elem cid . chainIds $ nodeVersion args =
    putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
  | otherwise = do
      cfg <- mkTXGConfig Nothing args host
      kps <- testSomeKeyPairs
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
    datum :: NEL.NonEmpty SomeKeyPairCaps -> Value
    datum kps = object ["test-admin-keyset" .= fmap (formatB16PubKey . fst) kps]

    f :: TXGConfig -> Command Text -> ExceptT ClientError IO ListenResponse
    f cfg cmd = do
      RequestKeys (rk NEL.:| _) <- ExceptT . pactSend cfg cid $ pure cmd
      ExceptT $ pactListen cfg cid rk


inMempool :: Args -> HostAddress -> (Sim.ChainId, [TransactionHash]) -> IO ()
inMempool args host (cid, txhashes)
    | not . elem cid . chainIds $ nodeVersion args =
      putStrLn "Invalid target ChainId" >> exitWith (ExitFailure 1)
    | otherwise = do
        cfg <- mkTXGConfig Nothing args host
        isMempoolMember cfg cid txhashes >>= \case
          Left e -> print e >> exitWith (ExitFailure 1)
          Right res -> pPrintNoColor res

work :: Args -> IO ()
work cfg = do
  tv  <- newTVarIO 0
  withLog $ \l -> forConcurrently_ (hostAddresses cfg) $ \host ->
    runLoggerT (act tv host) l
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ Info
    withLog inner = Y.withHandleBackend_ id (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

    act :: TVar TXCount -> HostAddress -> LoggerT T.Text IO ()
    act tv host@(HostAddress h p) = localScope (const [(hostnameToText h, portToText p)]) $
      case scriptCommand cfg of
        DeployContracts [] -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg host $ NEL.cons (initAdminKeysetContract v) (defaultContractLoaders v)
        DeployContracts cs -> liftIO $ do
          let v = nodeVersion cfg
          loadContracts cfg host $ initAdminKeysetContract v NEL.:| map (createLoader v) cs
        RunStandardContracts distribution ->
          realTransactions cfg host tv distribution
        RunCoinContract distribution ->
          realCoinTransactions cfg host tv distribution
        RunSimpleExpressions distribution ->
          simpleExpressions cfg host tv distribution
        PollRequestKeys rk -> liftIO $ pollRequestKeys cfg host
          . RequestKey
          . H.Hash
          . T.encodeUtf8
          $ rk
        ListenerRequestKey rk -> liftIO $ listenerRequestKey cfg host
          . ListenerRequest
          . RequestKey
          . H.Hash
          . T.encodeUtf8
          $ rk
        SingleTransaction stx -> liftIO $
          singleTransaction cfg host stx
        MempoolMember req -> liftIO $ inMempool cfg host req


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
  adminKS <- testSomeKeyPairs
  -- TODO: theData may change later
  let theData = object
          ["admin-keyset" .= fmap (formatB16PubKey . fst) adminKS
          , T.append (T.pack contractName) "-keyset" .= fmap (formatB16PubKey . fst) kp
          ]
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
