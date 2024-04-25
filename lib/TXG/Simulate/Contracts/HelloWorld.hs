{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Module: Main
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emamnuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module TXG.Simulate.Contracts.HelloWorld where

import           Data.Aeson
import           Data.Default
import qualified Data.List.NonEmpty as NEL
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           NeatInterpolation
import           Pact.ApiReq (mkExec)
import           Pact.Types.Capability (SigCapability)
import           Pact.Types.ChainId
import           Pact.Types.ChainMeta (PublicMeta(..))
import           Pact.Types.Command (Command(..), DynKeyPair)
import           Text.Printf
import           TXG.Fake
import           TXG.Simulate.Utils
import           TXG.Utils

---

helloWorldContractLoader
    :: ChainwebVersion
    -> PublicMeta
    -> NEL.NonEmpty (DynKeyPair,[SigCapability])
    -> IO (Command Text)
helloWorldContractLoader v meta adminKS = do
  let theData = object ["admin-keyset" .= fmap (formatPubKeyForCmd . fst) adminKS]
  mkExec theCode theData meta (NEL.toList adminKS) mempty (Just $ NetworkId $ chainwebVersionToText v) Nothing
  where
    theCode = [text|
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name])))
|]

newtype Name = Name { getName :: Text }
    deriving (Eq, Show, Generic)

instance Fake Name where
  fake = Name <$> personName

helloRequest :: ChainwebVersion -> Name -> IO (Command Text)
helloRequest v (Name name) = mkExec theCode theData def [] mempty (Just $ NetworkId $ chainwebVersionToText v) Nothing
  where
    theData = Null
    theCode = T.pack $ printf "(helloWorld.hello \"%s\")" name
