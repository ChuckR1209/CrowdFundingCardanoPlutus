{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week06.Token.OnChain
    ( tokenPolicy
    , tokenCurSymbol
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Plutarch.Context 

data FlattenedValueT = FlattenedValueT {vCurrencySymbol:: CurrencySymbol, vTokenName :: TokenName, vAmount :: Integer}
data Value = Value Map CurrencySymbol Map (TokenName, Integer)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

    checkInputsForTwoAda :: Bool
    checkInputsForTwoAda = any (\i -> txOutValue (txInInfoResolved i) == twoAdaValue) $ txInfoInputs info


{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == 1
        _                -> False

data MintRedeemer = Mint | Burn 
PlutusTx.unstableMakeIsData ``MintRedeemer

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> MintRedeemer -> ScriptContext -> Bool
mkTokenPolicy oref rdmr ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount &&
                                   case rdmr of 
                                    Mint -> True 
                                    _ -> False 

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt')] -> _ == tn && amt' == 1
        _                -> False

mkTokenPolicyWrapped :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkTokenPolicyWrapped oref rdmr ctx = mkTokenPolicy oref (unsafeToBuiltinData rdmr) ctx 
    



{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkTokenPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt')] -> _ == tn && amt' == 1
        _                -> False

--------------------------------------------------------------------------------
-- Original

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

mkValidatorWrapped :: BuiltinData -> () -> ScriptContext -> Bool
mkValidatorWrapped dat () ctx = mkValidator (unsafeToBuiltinData dat) () ctx 

--------------------------------------------------------------------------------
-- Optimized

data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> () -> SpookyScriptContext -> Bool
mkValidator dat' () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: STxInfo
    info = scriptContextTxInfo' ctx

    dat :: VestingDatum
    dat = (unsafeToBuiltinData dat') 

    signedByBeneficiary :: Bool
    signedByBeneficiary :: elem (unPaymentPubKeyHash $ beneficiary dat) (unSpooky (txInfoSignatories' info))
    -- signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ unSpooky (txInfoValidRange' info)
    -- deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info


newtype Spooky a = Spooky BuiltinData
  deriving stock (Eq, Ord, Show)
  deriving newtype (UnsafeFromData, FromData, ToData)

unSpooky ::
  UnsafeFromData a =>
  Spooky a ->
  a
unSpooky (Spooky a) = unsafeFromBuiltinData a
{-# INLINE unSpooky #-}

data SScriptPurpose
  = Minting (Spooky CurrencySymbol)
  | Spending (Spooky TxOutRef)
  | Rewarding (Spooky StakingCredential)
  | Certifying (Spooky DCert)
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

data SpookyScriptContext = SpookyScriptContext
  { scriptContextTxInfo' :: Spooky STxInfo,
    scriptContextPurpose' :: Spooky SScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq)

data STxInfo = STxInfo
  { -- | Transaction inputs
    txInfoInputs' :: Spooky [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs' :: Spooky [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee' :: Spooky Value,
    -- | The 'Value' minted by this transaction.
    txInfoMint' :: Spooky Value,
    -- | Digests of certificates included in this transaction
    txInfoDCert' :: Spooky [DCert],
    -- | Withdrawals
    txInfoWdrl' :: Spooky [(StakingCredential, Integer)],
    -- | The valid range for the transaction.
    txInfoValidRange' :: Spooky POSIXTimeRange,
    -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories' :: Spooky [PubKeyHash],
    txInfoData' :: Spooky [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId' :: Spooky TxId
  }
  deriving stock (Generic, Haskell.Eq)
--------------------------------------------------------------------------------

mkBuyValidator :: NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts r ctx =
    case r of
        Buy   -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                 traceIfFalse "Seller not paid" checkSellerOut &&
                 traceIfFalse "Fee not paid" checkMarketplaceFee &&
                 traceIfFalse "Royalities not paid" checkRoyaltyFee &&
                 traceIfFalse "More than one script input" onlyOneScriptInput
        Close -> traceIfFalse "No rights to perform this action" checkCloser
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = nToken nfts

    cs :: CurrencySymbol
    cs = nCurrency nfts

    pkh :: PubKeyHash 
    pkh = "deadbeef"
-- ...

--------------------------------------------------------------------------------

tokenPolicyTest :: Bool
tokenPolicyTest = mkTokenPolicy (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0) "Hello" 5 () TokenPolicyScriptContext

tokenPolicyTestExpectFail :: Bool
tokenPolicyTestExpectFail = mkTokenPolicy (TxOutRef "0b2086cbf8b6900a8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0) "Hello" 5 () TokenPolicyScriptContext

succeeds :: Bool -> Bool
succeeds evalResult = evalResult

fails :: Bool -> Bool
fails = not evalResult

unitTests :: TestTree
unitTests = testGroup "mintingPolicyTests"
    [
        succeeds tokenPolicyTest,
        fails tokenPolicyTestExpectFail
    ]

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn


typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

vestingScrAddress :: Ledger.Address
vestingScrAddress = scriptAddress validator

vestingInput :: Value -> VestingDatum -> Integer -> TxInInfo
vestingInput val vdat index = 
    TxInInfo
        (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" index)
        TxOut
            { txOutAddress   = vestingScrAddress 
            , txOutValue     = val
            , txOutDatumHash = Just (toDatumHash vdat)
            }

feeInput :: Address -> Integer -> TxInInfo
feeInput userAddr index = 
    TxInInfo
        (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" index)
        TxOut
            { txOutAddress   = userAddr 
            , txOutValue     = Value.singleton "" "" 2_000_000
            , txOutDatumHash = Nothing
            }

vestingOutput :: Value -> VestingDatum -> TxOut
vestingOutput val newDatum = 
    TxOut
        { txOutAddress   = vestingScrAddress
        , txOutValue     = val 
        , txOutDatumHash = Just (toDatumHash newDatum)
        }

changeOutput :: Address -> Value -> TxOut
changeOutput ownerAddr val =
    TxOut
        { txOutAddress   = ownerAddr
        , txOutValue     = val 
        , txOutDatumHash = Nothing
        }

mockTime :: POSIXTime
mockTime = 200_000

vestingBaseCtxBuilder :: SpendingBuilder
vestingBaseCtxBuilder = 
    mconcat
        [ input $
            script vestingScrAddress
            <> withValue scriptInputVal 
            <> withDatum (VestingDatum {beneficiary = beneficiary, deadline = deadline})
            <> withRef sref
        , input $ 
            pubKey ownerAddr
            <> withValue feeVal
        , withSpendingOutRef sref
        ]

vestingWithdrawBeforeDeadlineCtx :: ScriptContext
vestingWithdrawBeforeDeadlineCtx =
    let builder = vestingBaseCtxBuilder 
        <> mconcat 
            [ timeRange $ Interval.interval 1000 (1000 + 20)
            , output $
                pubKey ownerAddr
                <> withValue feeVal <> scriptInputVal
            ] in buildSpending mempty (mkNormalized builder)

vestingWrongPersonWithdraw :: ScriptContext
vestingWrongPersonWithdraw = 
    let builder = vestingBaseCtxBuilder 
        <> mconcat 
            [ timeRange $ Interval.interval mockTime (mockTime + 20_000)
            , output $
                pubKey (Address (PubKeyCredential "deadbeef") Nothing)
                <> withValue feeVal <> scriptInputVal
            ] in buildSpending mempty (mkNormalized builder)

vestingTestScriptContext :: ScriptContext 
vestingTestScriptContext = 
    let builder :: SpendingBuilder
    builder = mconcat
        [ input $
            script vestingScrAddress
            <> withValue scriptInputVal 
            <> withDatum (VestingDatum {beneficiary = beneficiary, deadline = deadline})
            <> withRef sref
        , input $ 
            pubKey ownerAddr
            <> withValue feeVal
        , output $
            pubKey ownerAddr
            <> withValue feeVal <> scriptInputVal
        , timeRange $ Interval.interval mockTime (mockTime + 20_000)
        , withSpendingOutRef sref
        ]
   in buildSpending mempty (mkNormalized builder)
   where
    scriptInputVal = (Value.singleton "deadbeef" "foo" 100) 
    feeVal = (Value.assetClassValue ada 2_000_000)
    sref = (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0)

templateTestScriptContext :: Address -> POSIXTime -> ScriptContext
templateTestScriptContext beneficiary deadline = 
        ScriptContext
            { scriptContextTxInfo  = TxInfo
                { txInfoInputs      = [ vestingInput vestingVal vestingDatum 0
                                      , feeInput ownerAddress 1 
                                      ]
                , txInfoOutputs     = [ changeOutput userAddress (Value.singleton "" "" 2_000_000 <> vestingval)]
                , txInfoFee         = Value.singleton "" "" 2
                , txInfoMint        = mempty
                , txInfoDCert       = []
                , txInfoWdrl        = []
                , txInfoValidRange  = Interval.interval mockTime (mockTime + 20_000)
                , txInfoSignatories = [ownerPubKeyHash]
                , txInfoData        = [(toDatumHash vestingDatum, vestingDatum)]
                , txInfoId          = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88bd"
                }
            , scriptContextPurpose = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0)
            }
    where
        ownerPubKeyHash :: PubKeyHash 
        ownerPubKeyHash = "99f1a8ae8528998a96f54fdef11ab0df50822dec6e403bbf21759e32"

        ownerAddress :: Address
        ownerAddress = Address (PubKeyCredential ownerPubKeyHash) Nothing

        vestingVal :: Value
        vestingVal = (Value.singleton "deadbeef" "foo" 100) 

        vestingDatum :: VestingDatum
        vestingDatum = VestingDatum {beneficiary = beneficiary, deadline = deadline}

vestingSucceedsTest :: Script
vestingSucceedsTest =
  applyValidator
    (Context (toBuiltinData (templateTestScriptContext beneficiary deadline)))
    typedValidator
    (Datum {getDatum = toBuiltinData vestingDatum})
    (Redeemer {getRedeemer = toBuiltinData ()})
  where
    vestingDatum = VestingDatum {beneficiary, deadline}
    beneficiary = Address (PubKeyCredential "99f1a8ae8528998a96f54fdef11ab0df50822dec6e403bbf21759e32") Nothing
    deadline = 1500

main :: IO ()
main = do
    let g = benchGroup
        "vesting benchmarks"
        [ [benchmarkScript "vesting size" $ getValidator typedValidator ]
        , [benchmarkScript "vesting succeeds test" $ vestingSucceedsTest ]
        ]

  PBoxes.printBox $ renderAdjBudgetTable g

--   let test1 = runScript (Context (toBuiltinData (templateTestScriptContext beneficiary deadline))) typedValidator (Datum {getDatum = toBuiltinData vestingDatum}) (Redeemer {getRedeemer = toBuiltinData ()})
--   print test1 
