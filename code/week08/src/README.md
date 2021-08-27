# Lecture 8 Chapter Links

0. YouTube video shortcuts
    * Shortcut overview: `?`
    * 10 sec forward / backward = `f` / `j`
    * 5 sec forward / backward = cursor left / cursor right
    * QWERTY: faster / slower = `>` / `<`
    * QWERTZ: faster / slower doesn't work out of the box
      * Install [Playspeed for Chrome](https://chrome.google.com/webstore/detail/playspeed/dgdpebjegmddbloaoiggfpdmkjmgiajj)
      * Works not only for YouTube
      * faster / slower = `s` / `d` 

1. [Token sale state machine: Introduction](https://youtu.be/JMRwkMgaBOg?t=52)
   1. [UTxO flow](https://youtu.be/JMRwkMgaBOg?t=67)
      ![UTxO flow >](./token-sale-utxo-flow.png)

   2. [`Token Sale`, `TSRedeemer`, `lovelaces`](https://youtu.be/JMRwkMgaBOg?t=263)
   3. [`transition`](https://youtu.be/JMRwkMgaBOg?t=354)
       1. [`SetPrice`](https://youtu.be/JMRwkMgaBOg?t=416)
       2. [`AddTokens`](https://youtu.be/JMRwkMgaBOg?t=510)
       3. [`BuyTokens`](https://youtu.be/JMRwkMgaBOg?t=580)
       4. [`Withdraw`](https://youtu.be/JMRwkMgaBOg?t=622)
   4. [`tsStateMachine`](https://youtu.be/JMRwkMgaBOg?t=699)
   5. [`mkTSValidator`, `tsTypedValidator`, `tsClient`, `mapErrorSM`](https://youtu.be/JMRwkMgaBOg?t=738)
   6. [`startTS`](https://youtu.be/JMRwkMgaBOg?t=821)
   7. [`setPrice`, `addTokens`, `buyTokens`, `withdraw`](https://youtu.be/JMRwkMgaBOg?t=948)
   8. [`TSStartSchema`, `TSUseSchema`](https://youtu.be/JMRwkMgaBOg?t=1020)
   9. [`startEndpoint`](https://youtu.be/JMRwkMgaBOg?t=1066)
   10. [`useEndpoints`](https://youtu.be/JMRwkMgaBOg?t=1114)
2. [Automatic testing using emulator traces](https://youtu.be/JMRwkMgaBOg?t=1145)
   1. [Tasty](https://youtu.be/JMRwkMgaBOg?t=1523)
3. [Optics](https://youtu.be/JMRwkMgaBOg?t=1976)
4. [Property based testing with QuickCheck](https://youtu.be/JMRwkMgaBOg?t=2849)
5. [Property based testing of Plutus Contracts](https://www.youtube.com/watch?v=JMRwkMgaBOg&t=3642s)
   1. [Model/System approach](https://youtu.be/JMRwkMgaBOg?t=3693)
      1. "System" is the real system
      2. "Model" is a model of the real system
   2. [Implementation imports](https://youtu.be/JMRwkMgaBOg?t=3870)
   3. [`TSState`, `TSModel`](https://youtu.be/JMRwkMgaBOg?t=3910)
   4. [`Action TSModel`](https://youtu.be/JMRwkMgaBOg?t=4014)
   5. [`ContractInstanceKey TSModel w s e`](https://youtu.be/JMRwkMgaBOg?t=4148)
   6. [`instanceTag key _`](https://youtu.be/JMRwkMgaBOg?t=4325)
   7. [`arbitraryAction _`, `genWallet`, `genNonNeg`](https://youtu.be/JMRwkMgaBOg?t=4507)
   8. [`initialState`](https://youtu.be/JMRwkMgaBOg?t=4913)
   9. [`nextState (Start w)`](https://youtu.be/JMRwkMgaBOg?t=4940)
      1. [`tokenCurrencies`, `nftCurrencies`, `tokenNames`, `tokens`, `nftAssets`, `nfts`, `tss`](https://youtu.be/JMRwkMgaBOg?t=5074)
      2. [`tsModel`](https://youtu.be/JMRwkMgaBOg?t=5194)
   10. [`nextState (SetPrice v w p)`](https://youtu.be/JMRwkMgaBOg?t=5338)
   11. [`nextState (AddTokens v w n)`, `getTSState'`, `getTSState`, `hasStarted`](https://youtu.be/JMRwkMgaBOg?t=5471)
   12. [`nextState (BuyTokens v w n)`](https://youtu.be/JMRwkMgaBOg?t=5887)
   13. [`nextState (Withdraw v w n l)`](https://youtu.be/JMRwkMgaBOg?t=5983)
   14. [`perform h _ cmd`, `delay`](https://youtu.be/JMRwkMgaBOg?t=6038)
   15. [`precondition`](https://youtu.be/JMRwkMgaBOg?t=6260)
   16. [`Eq`, `Show`](https://youtu.be/JMRwkMgaBOg?t=6330)
   17. [`instanceSpec`](https://youtu.be/JMRwkMgaBOg?t=6382)
   18. [`prop_TS`](https://youtu.be/JMRwkMgaBOg?t=6474)
   19. [Running the QuickCheck based tests](https://youtu.be/JMRwkMgaBOg?t=6614)
   20. [`test`](https://youtu.be/JMRwkMgaBOg?t=6696)
   21. [Intruduce a bug](https://youtu.be/JMRwkMgaBOg?t=6715)
   22. [Limitations](https://youtu.be/JMRwkMgaBOg?t=6863)
   23. [Integration with Tasty: `testProperty`](https://youtu.be/JMRwkMgaBOg?t=6999)
   24. [Cabal file](https://youtu.be/JMRwkMgaBOg?t=7022)
   25. [`Spec.hs`, `cabal test`](https://youtu.be/JMRwkMgaBOg?t=7077)
6. [Homework](https://www.youtube.com/watch?v=JMRwkMgaBOg&t=7123s)
