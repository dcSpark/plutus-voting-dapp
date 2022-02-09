{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BasicScenarios
  ( useCaseTests,
  )
where

import Control.Lens
import Control.Monad hiding (fmap)
import Endpoints
import Ledger.Ada as Ada
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import MaliciousEndpoints as ME
import Offchain
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Test.Tasty
import Wallet.Emulator.Wallet
import Prelude ((<>))

useCaseTests :: TestTree
useCaseTests =
  let options = defaultCheckOptions & emulatorConfig .~ emCfg
   in testGroup
        "Voting scenarios"
        [ checkPredicateOptions
            options
            "Voted wallet with quorum should get content of treasury"
            (assertNoFailedTransactions .&&. walletFundsChange w7 (Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue))
            voteWithQuorum,
          checkPredicateOptions
            options
            "Voted wallets with no quorum should not get content of treasury"
            (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError [" **** Not enough votes", "PT5"] _) -> True; _ -> False))
            voteWithNoQuorum
        ]

voteWithQuorum :: Trace.EmulatorTrace ()
voteWithQuorum = do
  h1 <- Trace.activateContractWallet w1 $ contract
  h2 <- Trace.activateContractWallet w2 $ contract
  h3 <- Trace.activateContractWallet w3 $ contract
  h4 <- Trace.activateContractWallet w4 $ contract
  h5 <- Trace.activateContractWallet w5 $ contract
  h6 <- Trace.activateContractWallet w6 $ contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1
  let voteParam = VoteAddressParams (mockWalletAddress w7) 1
  Trace.callEndpoint @"2-vote address" h2 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h3 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h4 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h5 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"3-collect" h6 ()
  void $ Trace.waitNSlots 1

voteWithNoQuorum :: Trace.EmulatorTrace ()
voteWithNoQuorum = do
  let evilContract = ME.endpoints voteCfg
  h1 <- Trace.activateContractWallet w1 $ contract
  h2 <- Trace.activateContractWallet w2 $ contract
  h3 <- Trace.activateContractWallet w3 $ contract
  h4 <- Trace.activateContractWallet w4 $ contract
  h5 <- Trace.activateContractWallet w5 $ contract
  h6 <- Trace.activateContractWallet w6 $ evilContract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1
  let voteParam1 = VoteAddressParams (mockWalletAddress w7) 1
  let voteParam2 = VoteAddressParams (mockWalletAddress w8) 1
  Trace.callEndpoint @"2-vote address" h2 voteParam1
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h3 voteParam1
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h4 voteParam2
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h5 voteParam2
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"collect-no-q" h6 ()
  void $ Trace.waitNSlots 1
