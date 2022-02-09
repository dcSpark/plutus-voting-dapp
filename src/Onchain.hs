{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Onchain where

import Data.List (partition)
import Ledger (Address (..), Datum (..), DatumHash, PubKeyHash (..), ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut (..), Value, scriptAddress, toValidatorHash, txOutValue, validatorHash)
import Ledger.Contexts qualified as Validation
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (AssetClass (..))
import Ledger.Value qualified as Value
import Playground.Contract
import PlutusTx qualified
import PlutusTx.AssocMap (fromList, member)
import PlutusTx.Prelude hiding ((<>))

-- UTILITY FUNCTIONS

-- onchain

-- the standard Maybe.fromJust don't work onchain
{-# INLINEABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a

{-# INLINEABLE datumToData #-}
datumToData :: (PlutusTx.FromData a) => Datum -> Maybe a
datumToData datum = PlutusTx.fromBuiltinData $ getDatum datum

{-# INLINEABLE validatorHashOf #-}
validatorHashOf :: TxInInfo -> Maybe ValidatorHash
validatorHashOf = toValidatorHash . txOutAddress . txInInfoResolved

{-# INLINEABLE datumHashOf #-}
datumHashOf :: TxInInfo -> DatumHash
datumHashOf = fromJust . txOutDatumHash . txInInfoResolved

{-# INLINEABLE valueOf #-}
valueOf :: TxInInfo -> Value
valueOf = txOutValue . txInInfoResolved

data VotingConfig = VotingConfig
  { voteAsset :: AssetClass,
    minQuorum :: Integer
  }
  deriving (Generic, Show)

PlutusTx.makeLift ''VotingConfig
PlutusTx.makeIsDataIndexed ''VotingConfig [('VotingConfig, 0)]

data VoteDatum = VoteDatum
  { votedAddress :: Address,
    weight :: Integer,
    owner :: PubKeyHash
  }
  deriving (Generic, Show)

PlutusTx.makeLift ''VoteDatum
PlutusTx.makeIsDataIndexed ''VoteDatum [('VoteDatum, 0)]

{-# INLINEABLE findExtractData #-}
findExtractData :: DatumHash -> TxInfo -> VoteDatum
findExtractData dh txInfo = fromJust (datumToData (fromJust (Validation.findDatum dh txInfo)))

-- ONCHAIN VALIDATORS

-- Vote script

data Vote

instance Scripts.ValidatorTypes Vote where
  type RedeemerType Vote = ()
  type DatumType Vote = VoteDatum

{-# INLINEABLE voteScript #-}
voteScript :: VotingConfig -> ValidatorHash -> VoteDatum -> () -> ScriptContext -> Bool
voteScript VotingConfig {voteAsset} treasury VoteDatum {owner = voteOwner, weight = votePower} _ ScriptContext {scriptContextTxInfo = txInfo@TxInfo {txInfoInputs}} =
  let buildVote weight = Value.assetClassValue voteAsset weight
      -- Spending path 1: The vote must be spent with the treasury in the same transaction
      -- (check that the treasury hash is present in the inputs)
      collectVotesAction = traceIfFalse " **** Must be spent together with the treasury" $ any (\txInInfo -> validatorHashOf txInInfo == Just treasury) txInfoInputs
      -- or
      -- Spending path 2: The vote must be returned to the owner
      returnVoteAction = traceIfFalse " **** Vote must be paid back to owner" $ Validation.valuePaidTo txInfo voteOwner == buildVote votePower
   in collectVotesAction || returnVoteAction

voteScriptInstance :: VotingConfig -> Scripts.TypedValidator Vote
voteScriptInstance votingConfig =
  Scripts.mkTypedValidator @Vote
    ($$(PlutusTx.compile [||voteScript||]) `PlutusTx.applyCode` PlutusTx.liftCode votingConfig `PlutusTx.applyCode` PlutusTx.liftCode treasuryHash)
    $$(PlutusTx.compile [||wrap||])
  where
    treasuryHash = treasuryScriptHash votingConfig
    wrap = Scripts.wrapValidator @VoteDatum @()

voteScriptAddress :: VotingConfig -> Address
voteScriptAddress votingConfig = Ledger.scriptAddress (Scripts.validatorScript $ voteScriptInstance votingConfig)

-- Treasury script
data Treasury

instance Scripts.ValidatorTypes Treasury where
  type RedeemerType Treasury = ()
  type DatumType Treasury = ()

{-# INLINEABLE treasuryScript #-}
treasuryScript :: VotingConfig -> () -> () -> ScriptContext -> Bool
treasuryScript VotingConfig {voteAsset, minQuorum} _ _ ctx@ScriptContext {scriptContextTxInfo = txInfo@TxInfo {txInfoInputs, txInfoOutputs}} =
  let -- split the inputs by partition looking for the input that has the hash of the current script (treasury)
      -- first, filter the wallet that pays the fee
      onlyScriptsUtxos = filter (isJust . validatorHashOf) txInfoInputs
      -- the result should be a list of votes (all the for the same address and amount) and one input from the treasury
      ([treasury], allVotes@(aVote : _)) = partition (\txInInfo -> Just (Validation.ownHash ctx) == validatorHashOf txInInfo) onlyScriptsUtxos
      -- extract the datum from the first vote
      aVoteDatum = fromJust (Validation.findDatum (datumHashOf aVote) txInfo)

      sumAllVotes = Value.assetClassValueOf value voteAsset where value = mconcat $ fmap (txOutValue . txInInfoResolved) allVotes

      -- validate all the votes have the same voted wallet and same payout
      compareDatums d1 d2 =
        let data1 = findExtractData d1 txInfo
            data2 = findExtractData d2 txInfo
         in votedAddress data1 == votedAddress data2

      allVotesAreTheSame =
        traceIfFalse " **** Not all votes are the same" $
          all (True ==) $
            fmap (compareDatums (datumHashOf aVote) . datumHashOf) allVotes

      -- validate the voted address is paid with the voted amount
      voteData = fromJust (datumToData aVoteDatum)
      votedAddressFromData = votedAddress voteData -- (pubKeyHashAddress (PaymentPubKeyHash $ votedAddress voteData) Nothing)
      valueInTreasury = valueOf treasury

      paidToVotedUtxos = filter (\TxOut {txOutAddress, txOutValue} -> txOutAddress == votedAddressFromData && txOutValue == valueInTreasury) txInfoOutputs
      ensureVotedIsPaid = traceIfFalse " **** Voted address is not paid the amount in treasury" $ length paidToVotedUtxos == 1

      -- validate the votes are preserved (input votes == output votes)
      outVotes = Validation.scriptOutputsAt (fromJust (validatorHashOf aVote)) txInfo
      inVotesDatumAndValues = fmap (\vote -> (datumHashOf vote, valueOf vote)) allVotes
      votesPreserved =
        let mapInVotes = fromList $ zip inVotesDatumAndValues inVotesDatumAndValues
            mapOutVotes = fromList $ zip outVotes outVotes
            inContainsAllOuts = all (`member` mapInVotes) mapOutVotes
            outContainsAllIns = all (`member` mapOutVotes) mapInVotes
         in traceIfFalse " **** Votes are not preserved: not all output votes have a corresponding input" inContainsAllOuts
              && traceIfFalse " **** Votes are not preserved: not all input votes have a corresponding output" outContainsAllIns

      -- validate enough votes
      quorumCheck = traceIfFalse " **** Not enough votes" (sumAllVotes >= minQuorum)
   in quorumCheck
        && allVotesAreTheSame
        && ensureVotedIsPaid
        && votesPreserved

treasuryScriptInstance :: VotingConfig -> Scripts.TypedValidator Treasury
treasuryScriptInstance votingConfig =
  Scripts.mkTypedValidator @Treasury
    ($$(PlutusTx.compile [||treasuryScript||]) `PlutusTx.applyCode` PlutusTx.liftCode votingConfig)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()

treasuryScriptHash :: VotingConfig -> ValidatorHash
treasuryScriptHash votingConfig = validatorHash $ Scripts.validatorScript $ treasuryScriptInstance votingConfig

treasuryScriptAddress :: VotingConfig -> Address
treasuryScriptAddress votingConfig = Ledger.scriptAddress (Scripts.validatorScript $ treasuryScriptInstance votingConfig)
