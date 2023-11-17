{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TXG.Simulate.Contracts.CoinContract where

import           Data.Aeson
import           Data.Bool
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Pact.ApiReq (mkExec)
import           Pact.Types.ChainId
import           Pact.Types.ChainMeta (PublicMeta(..))
import           Pact.Types.Command (Command(..), SomeKeyPairCaps)
import           System.Random
import           Text.Printf
import           TXG.Simulate.Contracts.Common
import           TXG.Simulate.Utils
import           TXG.Utils
import           TXG.Fake

---

data CoinContractRequest
  = CoinCreateAccount Account Guard
  | CoinAccountBalance Account
  | CoinTransfer SenderName ReceiverName Amount
  | CoinTransferAndCreate SenderName ReceiverName Guard Amount
  deriving Show

newtype Guard = Guard (NEL.NonEmpty SomeKeyPairCaps)
newtype SenderName = SenderName Account
newtype ReceiverName = ReceiverName Account

instance Show Guard where
    show _ = "<guard>"

instance Show SenderName where
    show (SenderName account) = "sender: " ++ show account

instance Show ReceiverName where
    show (ReceiverName account) = "sender: " ++ show account


mkRandomCoinContractRequest
    :: Bool
    -> M.Map Account (NEL.NonEmpty SomeKeyPairCaps)
    -> IO (FGen CoinContractRequest)
mkRandomCoinContractRequest transfersPred kacts = do
    request <- bool (randomRIO @Int (0, 1)) (return 1) transfersPred
    pure $ case request of
      0 -> CoinAccountBalance <$> fake
      1 -> do
          (from, to) <- distinctPairsSendersOverList (M.keys kacts)
          case M.lookup to kacts of
              Nothing -> error $ errmsg ++ getAccount to
              Just _keyset -> CoinTransfer
                  (SenderName from)
                  (ReceiverName to)
                  <$> fake
      _ -> error "mkRandomCoinContractRequest: impossible case"
    where
      errmsg =
        "mkRandomCoinContractRequest: something went wrong." ++
        " Cannot find account name: "


createCoinContractRequest
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty SomeKeyPairCaps
    -> CoinContractRequest
    -> IO (Command Text)
createCoinContractRequest v meta ks request =
    case request of
      CoinCreateAccount (Account account) (Guard g) -> do
        let theCode =
              T.pack $
              printf
              "(coin.create-account \"%s\" (read-keyset \"%s\"))"
              account
              ("create-account-guard" :: String)
            theData =
              object
                [ "create-account-guard" .= fmap (formatB16PubKey . fst) g
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ chainwebVersionToText v) Nothing
      CoinAccountBalance (Account account) -> do
        let theData = Null
            theCode =
              T.pack $
              printf
              "(coin.get-balance \"%s\")"
              account
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ chainwebVersionToText v) Nothing
      CoinTransferAndCreate (SenderName (Account sn)) (ReceiverName (Account rn)) (Guard g) (Amount amount) -> do
        let theCode =
              T.pack $
              printf
              "(coin.transfer-create \"%s\" \"%s\" (read-keyset \"%s\") %f)"
              sn
              rn
              ("receiver-guard" :: String)
              (fromRational @Double $ toRational amount)
            theData =
              object
                [ "receiver-guard" .= fmap (formatB16PubKey . fst) g
                ]
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ chainwebVersionToText v) Nothing

      CoinTransfer (SenderName (Account sn)) (ReceiverName (Account rn)) (Amount amount) -> do
        let theCode =
              T.pack $
              printf
              "(coin.transfer \"%s\" \"%s\" %f)"
              sn
              rn
              -- Super janky, but gets the job done for now
              (fromRational @Double $ toRational amount)
            theData = object []
        mkExec theCode theData meta (NEL.toList ks) (Just $ NetworkId $ chainwebVersionToText v) Nothing
