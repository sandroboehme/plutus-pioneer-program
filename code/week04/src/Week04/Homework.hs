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

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx

payContractErrorHandled :: Contract () PaySchema Text ()
payContractErrorHandled = do
    -- this is the handle func that gets error messages of type e
    -- the `Contract.logError` function returns a contract with error messages of e'
    let errFun = \err -> Contract.logError $ "caught error: " ++ unpack err
    Contract.handleError errFun payContract
    Contract.handleError errFun payContract
    -- 'payContract' is the contract I want to handle errors for.
    -- That contract has error messages of type e

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amount1 amount2 = do
    h <- activateContractWallet (Wallet 1) payContractErrorHandled

    let wallet2PubKeyHash = pubKeyHash $ walletPubKey $ Wallet 2

    callEndpoint @"pay" h PayParams { ppRecipient = wallet2PubKeyHash, ppLovelace  = amount1 }
    void $ Emulator.waitNSlots 1

    callEndpoint @"pay" h PayParams { ppRecipient = wallet2PubKeyHash, ppLovelace  = amount2 }
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
