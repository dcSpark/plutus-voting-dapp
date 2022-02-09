{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Control.Lens (review)
import Data.List (groupBy, maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Text qualified as T
import Ledger (Address (..), ChainIndexTxOut (..), Datum (..), DatumHash, PubKeyHash (..), Redeemer (..), Value, _ciTxOutDatum)
import Ledger.Constraints
import Ledger.Value (AssetClass, CurrencySymbol)
import Ledger.Value qualified as Value
import Onchain
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Types (AsContractError (_ConstraintResolutionError))
import PlutusTx qualified
import PlutusTx.Prelude
  ( Applicative (pure),
    Either,
    Maybe (..),
    const,
    either,
    error,
    fromMaybe,
    maybe,
    ($),
    (.),
    (>>=),
  )
import PlutusTx.Prelude hiding ((<>))
import Prelude qualified

findMostVotedGroupGeneric :: Prelude.Ord c => ([a] -> c) -> (a -> a -> Ordering) -> [a] -> [a]
findMostVotedGroupGeneric groupEval orderer elements =
  let -- create an equality comparation from an ordering
      grouper a b = isEq (a `orderer` b)
        where
          isEq EQ = True
          isEq _ = False
      -- group by equality ( sorting first, as it only groups adjacents)
      groups = groupBy grouper $ sortBy orderer elements
   in -- get the "top" list
      maximumBy (comparing groupEval) groups

-- instance PlutusTx.Ord Address where
findMostVotedGroup :: AssetClass -> [(TxOutRef, ChainIndexTxOut)] -> [(TxOutRef, ChainIndexTxOut)]
findMostVotedGroup voteAsset = findMostVotedGroupGeneric (sumVotes voteAsset) compareWalletAndPayout
  where
    compareWalletAndPayout (_, x) (_, y) = address' x `Prelude.compare` address' y
    address' x = fmap votedAddress $ unsafeDatum x

-- offchain

getDatumOrHash :: ChainIndexTxOut -> Either DatumHash Datum
getDatumOrHash PublicKeyChainIndexTxOut {} = throwError "no datum for a txout of a public key address"
getDatumOrHash ScriptChainIndexTxOut {_ciTxOutDatum} = _ciTxOutDatum

emptyRedeemer :: Redeemer
emptyRedeemer = Redeemer $ PlutusTx.toBuiltinData ()

getDatum' :: DatumHash -> Contract w s T.Text Datum
getDatum' dh =
  datumFromHash dh >>= \case
    Nothing -> throwError "datum not found"
    Just d -> pure d

extractData :: (PlutusTx.FromData a) => ChainIndexTxOut -> Contract w s T.Text a
extractData o = do
  (Datum e) <- either getDatum' pure $ getDatumOrHash o
  maybe
    (throwError "datum hash wrong type")
    pure
    (PlutusTx.fromBuiltinData e)

unsafeDatum :: (PlutusTx.FromData a) => ChainIndexTxOut -> Maybe a
unsafeDatum ciTxOut = either (const Nothing) datumToData $ getDatumOrHash ciTxOut

extractAddress :: ChainIndexTxOut -> Contract w s T.Text Address
extractAddress tx = do
  dat <- extractData tx
  pure $ votedAddress dat

extractOwner :: ChainIndexTxOut -> Contract w s T.Text PubKeyHash
extractOwner tx = do
  dat <- extractData tx
  pure $ owner dat

extractComponent :: Value -> CurrencySymbol -> TokenName -> Value
extractComponent value currency token = Value.singleton currency token $ Value.valueOf value currency token

sumVotes :: AssetClass -> [(TxOutRef, ChainIndexTxOut)] -> Integer
sumVotes voteAsset votes = Value.assetClassValueOf value voteAsset where value = mconcat $ fmap (_ciTxOutValue . snd) votes

mkMultiValidatorTx :: forall w s e. (AsContractError e) => [SomeLookupsAndConstraints] -> Contract w s e UnbalancedTx
mkMultiValidatorTx lookupsAndConstraints = either (throwError . review _ConstraintResolutionError) pure $ mkSomeTx lookupsAndConstraints
