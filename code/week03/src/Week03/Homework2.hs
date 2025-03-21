{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.Homework2 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf          (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkValidator p dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                           traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ p

    deadlineReached :: Bool
    deadlineReached = contains (from dat) $ txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = POSIXTime
    type instance RedeemerType Vesting = ()

-- I guess this is not needed as there is already a Lift instance for the PubKeyHash
-- PlutusTx.makeLift ''PubKeyHash

-- The syntax of a type is compiled into a syntax of something of type compiled code a
-- Generates Haskell code from Plutus Haskell code at compile time
-- "Exp Int" is a piece of code that represents an Int
-- mkValidator is a Haskell function it's not code
-- [|| mkValidator ||] uses the oxford brackets to return the underlaying syntax tree for the mkValidator function
-- "PlutusTx.compile [|| mkValidator ||]" compiles this syntax tree and returns the Plutus Core syntax tree for it.
-- But "Scripts.mkTypedValidator" doesn't expect a syntax but an actual script
-- The $$ is a so called splice. It takes a syntax tree and splices it into the source code at that point
-- Then the "Scripts.mkTypedValidator" takes this and turns it into a validator
-- I guess this validator is then a plain Haskell function way of expressing Plutus Core

-- "mkValidator p" would return the partially applied function with the missing params:
-- datum, redeemer and context
typedValidator :: PubKeyHash -> Scripts.TypedValidator Vesting
-- The p is not known at compile time but only at runtime when the actual endpoint is invoked
-- But template Haskell needs to know all the types inline at compile time.
-- "mkValidator p" would have the right type
-- <asdf> is not a Haskell function but a Plutus Core function
typedValidator p = Scripts.mkTypedValidator @Vesting
    -- "`PlutusTx.applyCode` PlutusTx.liftCode p" is Plutus Core code that compiles p
    -- into a syntax tree. This works as p is not arbitrary Haskell code with function types
    -- but only data or a data-like type. In this cases instances of the "Lift" class are available
    -- to be used as a parameter for "liftCode".
    -- "liftCode" creates "CompiledCode" for p which should be the Plutus Core syntax tree.
    -- "applyCode" should apply the mkValidator syntax tree to the one from the lift code.
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @POSIXTime @()

-- untyped validated based on the typed validator
validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator
-- the more explicit version without function composition looks like this:
-- validator p = Scripts.validatorScript $ typedValidator p

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p  = gpBeneficiary gp
        d  = gpDeadline gp
        tx = mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter (isSuitable now) <$> utxoAt (scrAddress pkh)
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos        <>
                          Constraints.otherScript (validator pkh)
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                          mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: POSIXTime -> TxOutTx -> Bool
    isSuitable now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
