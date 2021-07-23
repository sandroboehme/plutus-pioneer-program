{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Wallet.Emulator
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Control.Monad.Freer.Extras as Extras

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    Contract.logInfo @String "hello from the payContract"
    Contract.logInfo pp
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    -- payContractErrorHandled

payContractErrorHandled :: Contract () PaySchema Text ()
payContractErrorHandled = do
    Contract.logInfo @String "First call of payContractErrorHandled"
    Contract.handleError
    -- this is the handle func that gets error messages of type e
    -- the `Contract.logError` function returns a contract with error messages of e'
      (\err -> Contract.logError $ "caught error: " ++ unpack err)
      payContract -- the contract I want to handle errors for. That contract has error messages of type e
    -- payContractErrorHandled
    Contract.logInfo @String "Second call of payContractErrorHandled"
    Contract.handleError
    -- this is the handle func that gets error messages of type e
    -- the `Contract.logError` function returns a contract with error messages of e'
      (\err -> Contract.logError $ "caught error: " ++ unpack err)
      payContract -- the contract I want to handle errors for. That contract has error messages of type e
    -- payContractErrorHandled

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amount1 amount2 = do
    let pp1 = PayParams
              { ppRecipient = (pubKeyHash $ walletPubKey $ Wallet 2)
              , ppLovelace  = amount1
              }
    let pp2 = PayParams
              { ppRecipient = (pubKeyHash $ walletPubKey $ Wallet 2)
              , ppLovelace  = amount2
              }
    h <- activateContractWallet (Wallet 1) payContractErrorHandled
    callEndpoint @"pay" h pp1
    n <- Emulator.waitNSlots 1
    Extras.logInfo $ show n
    callEndpoint @"pay" h pp2
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
