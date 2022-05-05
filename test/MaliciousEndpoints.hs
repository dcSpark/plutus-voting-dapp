{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MaliciousEndpoints where

import Control.Lens (review)
import Control.Monad (forever, void)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Semigroup
import Data.Text qualified as T
import Ledger (toPubKeyHash, toTxOut, toValidatorHash, unitDatum, _ciTxOutValue)
import Ledger.Address (PaymentPubKeyHash (..))
import Ledger.Constraints
import Ledger.Contexts (TxOut (..))
import Onchain
import Plutus.Contract as Contract
import PlutusTx.Prelude hiding ((<>))
import Utils
import Prelude (zip3)

-- Tallying votes without Quorum
tallyNoQuorum :: VotingConfig -> Contract () MaliciousSchema T.Text ()
tallyNoQuorum vCfg@VotingConfig {voteAsset} = do
  votesUtxo <- utxosAt (voteScriptAddress vCfg)
  treasuryUtxo <- utxosAt $ treasuryScriptAddress vCfg
  let utxoList = Map.toList votesUtxo
      winningVotes = findMostVotedGroup voteAsset utxoList
      winningUtxos = Map.fromList winningVotes

  winningAddress <- extractAddress (snd $ head winningVotes)
  let voteScriptHash = fromJust $ toValidatorHash $ voteScriptAddress vCfg

      -- collect from the winning utxos and the treasury
      txVotesUtxos = collectFromScript winningUtxos ()
      txInputTreasury = collectFromScript treasuryUtxo ()

      -- pay the voted amount from the treasury (and keep the remainder in the treasury)
      totalTreasury = foldMap _ciTxOutValue $ Map.elems treasuryUtxo

      -- pay to either a public key or a script address,
      payWinner (Just winningPkh) _ = mustPayToPubKey (PaymentPubKeyHash winningPkh) totalTreasury
      -- We use an empty datum, but we can require a datum from the voters or lookup an existing datum
      payWinner _ (Just winningScript) = mustPayToOtherScript winningScript unitDatum totalTreasury
      payWinner _ _ = error ()

      txPayWinner = payWinner (toPubKeyHash winningAddress) (toValidatorHash winningAddress)

  -- rebuild spent votes

  datums <- mapM (either getDatum' pure) $ map (getDatumOrHash . snd) winningVotes
  let rebuildVote ((_, utxo), datum) = mustPayToOtherScript voteScriptHash datum $ txOutValue (toTxOut utxo)
      txRebuildVotes = foldMap rebuildVote $ zip winningVotes datums

      -- treasury script constraints
      treasuryUtxosConstraint = txInputTreasury <> txPayWinner
      treasuryLookups = typedValidatorLookups (treasuryScriptInstance vCfg) <> unspentOutputs treasuryUtxo

      -- vote script constraints
      votesUtxosConstraint = txVotesUtxos <> txRebuildVotes
      votesLookups = typedValidatorLookups (voteScriptInstance vCfg) <> unspentOutputs votesUtxo

      -- manually build the different script constraints
      treasurySpend = SomeLookupsAndConstraints treasuryLookups treasuryUtxosConstraint
      voteSpend = SomeLookupsAndConstraints votesLookups votesUtxosConstraint
      tx = mkSomeTx [treasurySpend, voteSpend]
  void $ do
    tx' <- either (throwError . review _ConstraintResolutionError) pure tx
    submitUnbalancedTx tx'

-- Returns another vote
returnOtherVote :: VotingConfig -> Contract () MaliciousSchema T.Text ()
returnOtherVote vCfg = do
  voter <- Contract.ownPaymentPubKeyHash
  voteUtxos <- utxosAt (voteScriptAddress vCfg)
  datums <- mapM (extractData . snd) $ Map.toList voteUtxos
  let voterPkh = unPaymentPubKeyHash voter
      voteUtxosList = Map.toList voteUtxos
      voteTxOut = map (toTxOut . snd) voteUtxosList
      votesToReturnUtxos = filter (\(_, datum, _) -> voterPkh == owner datum) $ zip3 voteTxOut datums voteUtxosList
      txPayToVoter = Foldable.fold $ map (\(txOut, _, _) -> mustPayToPubKey voter (txOutValue txOut)) votesToReturnUtxos
      votesToCollect = Map.fromList $ map (\(_, _, chainUtxo) -> chainUtxo) votesToReturnUtxos
      txVotesUtxos = collectFromScript votesToCollect ()
      tx = txPayToVoter <> txVotesUtxos
      lookups = typedValidatorLookups (voteScriptInstance vCfg)
  void $ mkTxConstraints @Vote lookups tx >>= submitTxConfirmed . adjustUnbalancedTx

type MaliciousSchema =
  Endpoint "collect-no-q" ()
    .\/ Endpoint "return any vote" ()

endpoints :: VotingConfig -> Contract () MaliciousSchema T.Text ()
endpoints vCfg =
  forever $
    handleError logError $
      awaitPromise $ tallyNoQuorum' `select` returnOtherVote'
  where
    tallyNoQuorum' = endpoint @"collect-no-q" $ \() -> tallyNoQuorum vCfg
    returnOtherVote' = endpoint @"return any vote" $ \() -> returnOtherVote vCfg
