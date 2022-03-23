{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Endpoints
  ( endpointTests,
    emCfg,
    tallyVotesTrace,
    contract,
    specialTokenValue,
    voteCfg,
  )
where

import Control.Lens
import Control.Monad hiding (fmap)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Ledger.Ada as Ada
import Ledger.Value qualified as Value
import Offchain as OC
import Onchain (VotingConfig (..))
import Plutus.Contract (Contract)
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import Test.Tasty
import Wallet.Emulator.Wallet (mockWalletAddress)

-- minimum number of agreement votes required
minQuorum' :: Integer
minQuorum' = 4

currency :: Value.CurrencySymbol
currency = "66"

token :: Value.TokenName
token = "VotingToken"

voteAsset' :: Value.AssetClass
voteAsset' = Value.assetClass currency token

aVoteValue :: Value.Value
aVoteValue = Value.assetClassValue voteAsset' 1

specialTokenValue :: Value.Value
specialTokenValue = Value.singleton currency "CONTROL_TOKEN" 1

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left $ Map.fromList ((w1, v <> specialTokenValue) : [(w, v) | w <- [w2, w3, w4, w5, w6, w7]])) def def
  where
    v = Ada.lovelaceValueOf 1_000_000_000_000 <> aVoteValue

voteCfg :: VotingConfig
voteCfg = VotingConfig {voteAsset = voteAsset', minQuorum = minQuorum'}

contract :: Contract () VotingSchema T.Text ()
contract = OC.endpoints voteCfg

endpointTests :: TestTree
endpointTests =
  let options = defaultCheckOptions & emulatorConfig .~ emCfg
      tag :: Trace.ContractInstanceTag
      tag = "instance 1"
   in testGroup
        "Simple endpoint tests"
        [ checkPredicate
            "Expose endpoints"
            ( endpointAvailable @"1-setup treasury" contract tag
                .&&. endpointAvailable @"2-vote address" contract tag
                .&&. endpointAvailable @"3-collect" contract tag
                .&&. endpointAvailable @"4-return vote" contract tag
            )
            $ void $ Trace.activateContractWallet w1 contract,
          checkPredicateOptions options "Build treasury" assertNoFailedTransactions buildTreasuryTrace,
          checkPredicateOptions options "Single vote" assertNoFailedTransactions singleVoteTrace,
          checkPredicateOptions options "Return vote" assertNoFailedTransactions returnVoteTrace,
          checkPredicateOptions options "Tally votes" assertNoFailedTransactions tallyVotesTrace
        ]

buildTreasuryTrace :: Trace.EmulatorTrace ()
buildTreasuryTrace = do
  h1 <- Trace.activateContractWallet w1 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1

singleVoteTrace :: Trace.EmulatorTrace ()
singleVoteTrace = do
  h1 <- Trace.activateContractWallet w1 contract
  h2 <- Trace.activateContractWallet w2 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1
  let voteParam = VoteAddressParams (mockWalletAddress w5) 1
  Trace.callEndpoint @"2-vote address" h2 voteParam

returnVoteTrace :: Trace.EmulatorTrace ()
returnVoteTrace = do
  h1 <- Trace.activateContractWallet w1 contract
  h2 <- Trace.activateContractWallet w2 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1
  let voteParam = VoteAddressParams (mockWalletAddress w5) 1
  Trace.callEndpoint @"2-vote address" h2 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"4-return vote" h2 ()
  void $ Trace.waitNSlots 1

tallyVotesTrace :: Trace.EmulatorTrace ()
tallyVotesTrace = do
  h1 <- Trace.activateContractWallet w1 contract
  h2 <- Trace.activateContractWallet w2 contract
  h3 <- Trace.activateContractWallet w3 contract
  h4 <- Trace.activateContractWallet w4 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
  void $ Trace.waitNSlots 1
  let voteParam = VoteAddressParams (mockWalletAddress w5) 1
  Trace.callEndpoint @"2-vote address" h2 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"2-vote address" h3 voteParam
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"3-collect" h4 ()
  void $ Trace.waitNSlots 1
