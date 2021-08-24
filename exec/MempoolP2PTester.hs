-- | Module: MempoolP2PTester
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Main ( main ) where


import           Control.Monad (unless)
import           Text.Printf
import           Text.Pretty.Simple (pPrintNoColor)

import           Configuration.Utils hiding (Error, Lens')
import           TXG.Types
import           TXG.Utils

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
  let chains = chainIds (nodeVersion config)
      isMem = all (`elem` chains) $ nodeChainIds config
  unless isMem $ error $
    printf "Invalid chain %s for given version\n" (show $ nodeChainIds config)
  pPrintNoColor config
  worker config

mainInfo :: ProgramInfo Args
mainInfo =
  programInfo
  "MempoolP2PTester"
  scriptConfigParser
  defaultArgs

worker :: Args -> IO ()
worker _config = return ()

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

1. the time that it takes until txs is accepted in the mempool (send returns requestkey)
2. the time that it takes until txs is included in a block
3. the time that it takes until txs reach confirmation depth

-}
