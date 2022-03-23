{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Offchain where

import Control.Monad (forever, void)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Semigroup
import Data.Text qualified as T
import Ledger (Address (..), Value, toPubKeyHash, toTxOut, toValidatorHash, unitDatum, _ciTxOutValue)
import Ledger.Address (PaymentPubKeyHash (..))
import Ledger.Constraints
import Ledger.Contexts (TxOut (..))
import Ledger.Value qualified as Value
import Onchain
import Playground.Contract
import Plutus.Contract as Contract
import PlutusTx.Prelude hiding ((<>))
import Utils
import Prelude (zip3)
import Prelude qualified

---------------------------------------------
-- Initialize the treasury with some value
setupTreasury :: VotingConfig -> Value -> Contract () VotingSchema T.Text ()
setupTreasury voteConfig treasuryAmount = do
  let tx = mustPayToTheScript () treasuryAmount
      treasury = treasuryScriptInstance voteConfig
  void (submitTxConstraints treasury tx)

-- | Parameters for the "vote address" endpoint
data VoteAddressParams = VoteAddressParams
  { votedFor :: Address,
    votes :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Vote for address
voteAddress :: VotingConfig -> VoteAddressParams -> Contract () VotingSchema T.Text ()
voteAddress vCfg@VotingConfig {voteAsset} (VoteAddressParams votedFor votes) = do
  voter <- Contract.ownPaymentPubKeyHash
  let buildVote weight = Value.assetClassValue voteAsset weight
      txAddVote = mustPayToTheScript VoteDatum {votedAddress = votedFor, weight = votes, owner = unPaymentPubKeyHash voter} $ buildVote votes
      lookups = typedValidatorLookups (voteScriptInstance vCfg)
  void $ mkTxConstraints @Vote lookups txAddVote >>= submitTxConfirmed . adjustUnbalancedTx
  logInfo $ "Voted for: " ++ Prelude.show votedFor

-- Tally votes endpoint
-- collect enough votes and spend the voted amount from the treasury to the winning choice

tally :: VotingConfig -> Contract () VotingSchema T.Text ()
tally vCfg@VotingConfig {voteAsset, minQuorum} = do
  votesUtxo <- utxosAt (voteScriptAddress vCfg)
  treasuryUtxo <- utxosAt $ treasuryScriptAddress vCfg
  if not (Map.null votesUtxo)
    then do
      let utxoList = Map.toList votesUtxo
          winningVotes = findMostVotedGroup voteAsset utxoList
          winningUtxos = Map.fromList winningVotes
          count = sumVotes voteAsset winningVotes

      if count >= minQuorum
        then do
          winningAddress <- extractAddress (snd $ head winningVotes)
          let voteScriptHash = fromJust $ toValidatorHash $ voteScriptAddress vCfg

              -- collect from the winning utxos and the treasury
              txVotesUtxos = collectFromScript winningUtxos ()
              txInputTreasury = collectFromScript treasuryUtxo ()

              -- datum = Datum $ PlutusTx.toData $ PlutusTx.toBuiltinData $ VoteDatum{votedFor=votedFor, weight=count, owner=collector}

              -- pay the voted amount from the treasury (and keep the remainder in the treasury)
              totalTreasury = foldMap _ciTxOutValue $ Map.elems treasuryUtxo

              -- pay to either a public key or a script address,
              payWinner (Just winningPkh) _ = mustPayToPubKey (PaymentPubKeyHash winningPkh) totalTreasury
              -- We use an empty datum, but we can require a datum from the voters or lookup an existing datum
              payWinner _ (Just winningScript) = mustPayToOtherScript winningScript unitDatum totalTreasury
              payWinner _ _ = error ()

              txPayWinner = payWinner (toPubKeyHash winningAddress) (toValidatorHash winningAddress)

          -- rebuild spent votes

          -- TODO properly rebuild datum
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
          void $ mkMultiValidatorTx [treasurySpend, voteSpend] >>= submitTxConfirmed . adjustUnbalancedTx
        else do
          throwError $ T.pack "Not enough votes"
    else do
      throwError $ T.pack "No votes found"

returnVote :: VotingConfig -> Contract () VotingSchema T.Text ()
returnVote vCfg = do
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

type VotingSchema =
  Endpoint "1-setup treasury" Value
    .\/ Endpoint "2-vote address" VoteAddressParams
    .\/ Endpoint "3-collect" ()
    .\/ Endpoint "4-return vote" ()

endpoints :: VotingConfig -> Contract () VotingSchema T.Text ()
endpoints vCfg =
  forever $
    handleError logError $
      awaitPromise $ setupTreasury' `select` voteAddress' `select` tally' `select` returnVote'
  where
    setupTreasury' = endpoint @"1-setup treasury" $ setupTreasury vCfg
    voteAddress' = endpoint @"2-vote address" $ voteAddress vCfg
    tally' = endpoint @"3-collect" $ \() -> tally vCfg
    returnVote' = endpoint @"4-return vote" $ \() -> returnVote vCfg
