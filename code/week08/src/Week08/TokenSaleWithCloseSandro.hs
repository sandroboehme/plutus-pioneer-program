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

module Week08.TokenSaleWithCloseSandro
    ( TokenSale (..)
    , TSRedeemer (..)
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsTT     :: !(Maybe ThreadToken)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

data TSRedeemer =
      SetPrice Integer -- token price in lovelace
    | AddTokens Integer -- amount of tokens to add
    | BuyTokens Integer -- amount of tokens to buy
    | Withdraw Integer Integer -- token amount, lovelace amount
    -- `Close` can only be called by the seller to close the contract / UTxO
    -- and collect the remaining tokens and lovelace and the NFT
    | Close
    deriving (Show, Prelude.Eq)

-- So for the model you could add a new action close,
-- For the emulator trace as well,
-- as a last step provide the close action.


PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
-- State Integer = The datum type of the state is `Integer` = the price of the token
-- Use `State c`
-- `Just Integer` means that the contract is still active
-- `Nothing` means it has been closed
transition :: TokenSale -> State (Maybe Integer) -> TSRedeemer -> Maybe (TxConstraints Void Void, State (Maybe Integer))
transition ts s r = case (stateValue s, stateData s, r) of
    (v, (Just _), SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State (Just p) v
                                                    )
    (v, (Just p), AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State (Just p) $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                      -- `assetClassValue :: AssetClass -> Integer -> Value`
                                                      -- A Value containing the given amount of the asset class.
                                                      -- http://localhost:8088/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#v:assetClassValue
                                                    )
    (v, (Just p), BuyTokens n)  | n > 0            -> Just ( mempty
                                                      -- p = price
                                                    , State (Just p) $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, (Just p), Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State (Just p) $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    (v, (Just _), Close)                           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                      , State Nothing mempty
                                                      )
    _                                              -> Nothing


{-# INLINABLE final #-}
final :: Maybe Integer -> Bool
final Nothing   = True
final _          = False

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
-- the last parameter `(const False)` indicates which states are final
-- the literal means that there are no final states
tsStateMachine ts = mkStateMachine (tsTT ts) (transition ts) final

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine (Maybe Integer) TSRedeemer


tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

startTS :: AssetClass -> Bool -> Contract (Last TokenSale) s Text ()
-- what is `useTT`?
startTS token useTT = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- I guess `useTT` is to make testing easier later - to use a thread token if one
    -- exists or to create one if none is provided
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client (Just 0) mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale (endpoint) " ++ show ts
    logInfo $ "Thread token: " ++ (show (tt))

setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

close :: TokenSale -> () -> Contract w s Text ()
close ts _ = void $ mapErrorSM $ runStep (tsClient ts) $ Close

type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName, Bool)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close contract" ()

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts = forever
                $ handleError logError
                $ awaitPromise
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close'
  where
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ uncurry (withdraw ts)
    close'     = endpoint @"close contract" $ close ts
