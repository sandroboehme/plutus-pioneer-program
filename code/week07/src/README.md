# Lecture 7 Chapter Links

0. YouTube video shortcuts
    * Shortcut overview: `?`
    * 10 sec forward / backward = `f` / `j`
    * 5 sec forward / backward = cursor left / cursor right
    * QWERTY: faster / slower = `>` / `<`
    * QWERTZ: faster / slower doesn't work out of the box
      * Install [Playspeed for Chrome](https://chrome.google.com/webstore/detail/playspeed/dgdpebjegmddbloaoiggfpdmkjmgiajj)
      * Works not only for YouTube
      * faster / slower = `s` / `d` 

1. [Even Odd Procedure](https://youtu.be/oJupInqvJUI?t=79)
    * Playing via email, snail, blockchain
    * Commit Scheme
    * Nonce

2. [Even Odd Procedure with nonce, State Machines](https://youtu.be/oJupInqvJUI?t=403)

3. [Naive implementation](https://youtu.be/oJupInqvJUI?t=548)

    1.[`Game`](https://youtu.be/oJupInqvJUI?t=548)

    2.[`GameChoice`, `Eq GameChoice`](https://youtu.be/oJupInqvJUI?t=676)

    3.[`GameDatum`, `Eq GameDatum`](https://youtu.be/oJupInqvJUI?t=724)

    4.[`GameRedeemer`, `lovelaces`](https://youtu.be/oJupInqvJUI?t=764)

    5.[`gameDatum`](https://youtu.be/oJupInqvJUI?t=846)

    6.[`mkGameValidator`](https://youtu.be/oJupInqvJUI?t=846)

    1. [`info`, `ownInput`](https://youtu.be/oJupInqvJUI?t=952)

    2. [`ownOutput`](https://youtu.be/oJupInqvJUI?t=969)

    3. [`outputDatum`](https://youtu.be/oJupInqvJUI?t=969)

    4. [`checkNonce`](https://youtu.be/oJupInqvJUI?t=1010)

    5. [`nftToFirst`](https://youtu.be/oJupInqvJUI?t=1116)

    6. [error conditions general](https://youtu.be/oJupInqvJUI?t=1176)

       1. [1st case: First player moved, the second player is moving now](https://youtu.be/oJupInqvJUI?t=1202)
       2. [2nd case: Both players moved and the first player discovers that he has won](https://youtu.be/oJupInqvJUI?t=1281)
       3. [3rd case: Second player didn't move within the deadline and first player wants to have his stake back](https://youtu.be/oJupInqvJUI?t=1321)
       4. [4th case: Both players moved. First player missed the deadline. ](https://youtu.be/oJupInqvJUI?t=1281)

    7.[`Gaming`, `bsZero`, `bsOne`](https://youtu.be/oJupInqvJUI?t=1410)

    8.[`gameInst`, `gameValidator`, `gameAddress`](https://youtu.be/oJupInqvJUI?t=1410)

    9.[`findGameOutput`](https://youtu.be/oJupInqvJUI?t=1464)

    10.[short off-chain overview](https://youtu.be/oJupInqvJUI?t=1593)

    11.[`FirstParams`](https://youtu.be/oJupInqvJUI?t=1610)

    12.[`firstGame`](https://youtu.be/oJupInqvJUI?t=1638)

    1. [after play-deadline passed: no game utxo found](https://youtu.be/oJupInqvJUI?t=1711)

    2. [after play-deadline passed: game utxo found: second player hasn't moved](https://youtu.be/oJupInqvJUI?t=1759)

    3. [after play-deadline passed: game utxo found: second player did move and won / lost](https://youtu.be/oJupInqvJUI?t=1796)

    13.[`SecondParams`](https://youtu.be/oJupInqvJUI?t=1846)

    14.[`secondGame`, no game utxo found](https://youtu.be/oJupInqvJUI?t=1869)

    1. [game utxo found](https://youtu.be/oJupInqvJUI?t=1908)

    2. [after reveal-deadline passed, no game utxo found](https://youtu.be/oJupInqvJUI?t=2013)

    3. [after reveal-deadline passed, game utxo found](https://youtu.be/oJupInqvJUI?t=2037)

   15.[`GameSchema`, `endpoints` contract](https://youtu.be/oJupInqvJUI?t=2089)

   16.[Test explanaition](https://youtu.be/oJupInqvJUI?t=2137)

   17.[Test run, Zero / Zero, Wallet 1 wins](https://youtu.be/oJupInqvJUI?t=2333)

   18.[Test run, Zero / One, Wallet 2 wins](https://youtu.be/oJupInqvJUI?t=2421)
   
4. [State machines, definitions](https://youtu.be/oJupInqvJUI?t=2483)
   * The **state machine** will be represented by the **UTxO sitting at the script address**.
   * The **state** will be the **datum of the UTxO**
   * The **transition** will be the **transaction that consumes the current state - the current UTxo**, using a **redeemer that characterizes the transition** and then produces a **new UTxO at the same address** where the **datum now reflects the new state**.

   1.[API, `StateMachine`](https://youtu.be/oJupInqvJUI?t=2635)
    1. [`smTransition` and `State`](https://youtu.be/oJupInqvJUI?t=2683)
    2. [`smFinal`, `smCheck`, `smThreadToken`](https://youtu.be/oJupInqvJUI?t=2760)

   2.[Even Odd with state machine, changes](https://youtu.be/oJupInqvJUI?t=2858)
   * `Game` and `GameChoice` stay the same `GameDatum` now additionally has a `Finished` constructor for the final state (won't correspond to an UTxO).
   * `Eq GameDatum` has changed for the final state as well.
   * `GameRedeemer`, `lovelaces`, `gameDatum` are the same

   3.[`transition`, short intro and comparison to non state machine version](https://youtu.be/oJupInqvJUI?t=2929)
    1. [1st case: First player moved, the second player is moving now](https://youtu.be/oJupInqvJUI?t=3028)

    2. [2nd case: Both players moved and the first player discovers that he has won](https://youtu.be/oJupInqvJUI?t=3028)

    3. [3rd case: Second player didn't move within the deadline and first player wants to have his stake back](https://youtu.be/oJupInqvJUI?t=3028)

    4. [4th case: Both players moved. First player missed the deadline.](https://youtu.be/oJupInqvJUI?t=3311)

    5.[`final`, no nonce check, `check` instead](https://youtu.be/oJupInqvJUI?t=3382)

   6.[`gameStateMachine`](https://youtu.be/oJupInqvJUI?t=3458)

   7.[`mkGameValidator`, `Gaming`, `bsZero`, `bsOne`, `gameStateMachine'`](https://youtu.be/oJupInqvJUI?t=3487)

   8.[`gameInst` is similar, `gameValidator` and `gameAddress` is same](https://youtu.be/oJupInqvJUI?t=3559)

   9.[`gameClient` to interact with the state machine from the wallet Monad (off-chain)](https://youtu.be/oJupInqvJUI?t=3579)

   10.[`FirstParams` same except the `SMContractError` error type, `mapError'` ](https://youtu.be/oJupInqvJUI?t=3698)

   11.[`firstGame`, using `pubKeyHash`, `gameClient`, `runInitialize` and `mapError'`](https://youtu.be/oJupInqvJUI?t=3745)

    1. [`findGameOutput` vs. `getOnChainState` after play-deadline passed](https://youtu.be/oJupInqvJUI?t=3840)

    2. [case on-chain not found / found, `runStep`](https://youtu.be/oJupInqvJUI?t=3840)

   12.[`secondGame`](https://youtu.be/oJupInqvJUI?t=4126)

   13.[How exactly is the state machine shorter?](https://youtu.be/oJupInqvJUI?t=4208)

   13.[Test is very similar code, Repl output](https://youtu.be/oJupInqvJUI?t=4305)

   13.[State machine use cases wrap up](https://youtu.be/oJupInqvJUI?t=4363)

5. [Homework](https://youtu.be/oJupInqvJUI?t=4442)





