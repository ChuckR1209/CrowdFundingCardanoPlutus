-- 1. dependecies
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 

-- 2. imports external/imports

module CrowdFunding.CrowdFundingOnChain where

import qualified Prelude                                as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada
-- import qualified Ledger.Constraints.TxConstraints                as Tx
import qualified Ledger.Value                                      as Value
import qualified Plutus.V2.Ledger.Tx                             as V2LedgerTx

-- data CrowdParam = CrowdParam
--     {
--         creator :: Ledger.PaymentPubKeyHash,

--     } deriving P.Show

-- PlutusTx.unstableMakeIsData ''CrowdParam
-- PlutusTx.makeLift ''CrowdParam

minLovelace :: Integer
minLovelace = 2000000

data Dat = Dat 
    {
        beneficiary :: Ledger.PaymentPubKeyHash
        , deadline :: LedgerApiV2.POSIXTime
        , aCurrency :: LedgerApiV2.CurrencySymbol
        , aToken    :: LedgerApiV2.TokenName
        , targetAmount :: Integer
        , contributorsMap :: [(Ledger.PaymentPubKeyHash,Integer)]
    } deriving P.Show
PlutusTx.unstableMakeIsData ''Dat


data Contribution 


data Redeem = Contribute 
    {
        contribution :: (Ledger.PaymentPubKeyHash,Integer)
    } 
              | Close 
    deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem

data Crowd
instance Scripts.ValidatorTypes Crowd where
    type instance RedeemerType Crowd = Redeem
    type instance DatumType Crowd = Dat

-- 3. onchain code
{-# INLINABLE crowdValidator #-}
crowdValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
crowdValidator d r context = 
    -- traceIfFalse "Failure to guess" (guess d == redeem r) &&
    traceIfFalse "wrong input value" ( correctInputValue d )
    -- traceIfFalse "wrong output datum" (correctBidOutputDatum d) &&
    -- traceIfFalse "Not signed by beneficiary" signedByBeneficiary &&
    -- traceIfFalse "Deadline not yet reached" deadlinepassed
    -- traceIfFalse "Royalties not provided" calculateRoyalties
    
    where 
        txinfo :: Contexts.TxInfo 
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

        deadlinepassed :: Bool 
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)

        inputsAll :: [Contexts.TxInInfo]
        inputsAll = Contexts.txInfoInputs txinfo

        inputsAllResolved :: [Contexts.TxOut]
        inputsAllResolved = fmap Contexts.txInInfoResolved inputsAll

        getAllValuesTxIns :: [Ledger.Value]
        getAllValuesTxIns = fmap Contexts.txOutValue inputsAllResolved


--      these below we will not use now. But code is there for going in to get All datums from all Tx-in for later use
        -- inputsAllResolvedDatums :: [V2LedgerTx.OutputDatum]
        -- inputsAllResolvedDatums = fmap Contexts.txOutDatum inputsAllResolved

        -- getValue :: [V2LedgerTx.OutputDatum] -> [V2LedgerTx.OutputDatum]
        -- getValue [] = []
        -- getValue [(V2LedgerTx.OutputDatum d)] = [V2LedgerTx.OutputDatum d]
        -- getValue [V2LedgerTx.NoOutputDatum] = []
        -- getValue [(V2LedgerTx.OutputDatumHash _)] = []       -- for now also ignore DatumHashs
        -- getValue ( ((V2LedgerTx.OutputDatum d)) : dts) = (V2LedgerTx.OutputDatum d) : (getValue  dts)
        -- getValue ( ((V2LedgerTx.OutputDatumHash _)) : dts) = (getValue  dts)
        -- getValue ( ((V2LedgerTx.NoOutputDatum )) : dts) = (getValue  dts)

        -- getFinalDatums :: [V2LedgerTx.OutputDatum]
        -- getFinalDatums = getValue inputsAllResolvedDatums


--      this below commented out section is from Auction.hs plutus pioneer. But now Maybe Datums have changed. 
        -- findMyDatum :: [V2LedgerTx.OutputDatum] -> Maybe V2LedgerTx.OutputDatum
        -- findMyDatum [] = Nothing
        -- findMyDatum x = 

        -- input :: Contexts.TxInInfo
        -- input =
        --   let
        --     isScriptInput i = case (Contexts.txOutDatum . Contexts.txInInfoResolved) i of
        --         Nothing -> False
        --         Just _  -> True
        --     xs = [i | i <- Contexts.txInfoInputs txinfo, isScriptInput i]
        --   in
        --     case xs of
        --         [i] -> i
        --         _   -> traceError "expected exactly one script input"

        -- inVal :: Ledger.Value
        -- inVal = Contexts.txOutValue . Contexts.txInInfoResolved $ input


        amountInDatum :: [(Ledger.PaymentPubKeyHash,Integer)] -> Integer
        -- amountInDatum = 10                    -- for now. but we need to loop throught the list of contributorsMap to get the total
        amountInDatum [] = 0
        amountInDatum [ (_, i)] = i
        amountInDatum ((_,i) : is) = i + amountInDatum is

        tokenValue :: Ledger.Value
        tokenValue = Value.singleton (aCurrency d) (aToken d) 1

--      We want to see if at least one of the Tx-in has our NFT Token and also matches the amount
--                             Also for now we are only checking NFT and amount but in theory we need to also check rest of fields too. --- later
        checkInputFound :: [Ledger.Value] -> Ledger.Value -> Bool
        checkInputFound [] _ = False
        checkInputFound [txV] dtV = txV == dtV 
        checkInputFound (txV : txs) dtV = txV == dtV  || (checkInputFound txs dtV)

        correctInputValue :: Dat -> Bool
        --    Left side is what's value from Tx-in and     right side is expected from Datum
        correctInputValue dt = checkInputFound getAllValuesTxIns (tokenValue <> Ada.lovelaceValueOf (minLovelace + ( amountInDatum (contributorsMap dt) )))    
--         correctInputValue dt = tokenValue == tokenValue <> Ada.lovelaceValueOf (minLovelace + ( amountInDatum (contributorsMap dt) ))    --- Temp
        -- correctInputValue = inVal == tokenValue <> Ada.lovelaceValueOf (minLovelace + amountInDatum) 


    
        -- adaroyalties :: Maybe Ada.Ada
        -- adaroyalties = do 
        --     validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
        --     Just $ Ada.fromValue validatedValue `Ada.divide` 10

        -- getValuePaidToCreator :: Ada.Ada 
        -- getValuePaidToCreator = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator crowdp))

        -- compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
        -- compareValues _ Nothing = False
        -- compareValues vToCreator adaroy = Just vToCreator >= adaroy

        -- calculateRoyalties :: Bool
        -- calculateRoyalties = compareValues getValuePaidToCreator adaroyalties

-- crowdCompile :: CrowdParam -> Scripts.TypedValidator Crowdf 
-- crowdCompile crowdp = Scripts.mkTypedValidator @Crowdf 
--     ($$(PlutusTx.compile [|| crowdValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode crowdp)
--     $$(PlutusTx.compile [|| wrap ||]) where 
--         wrap = Scripts.mkUntypedValidator @Dat @Redeem

crowdCompile :: Scripts.TypedValidator Crowd
crowdCompile = Scripts.mkTypedValidator @Crowd
    $$(PlutusTx.compile [|| crowdValidator ||]) 
    $$(PlutusTx.compile [|| wrap ||]) where 
        wrap = Scripts.mkUntypedValidator @Dat @Redeem
--------------------------------OnChain code


--------------------------------
validator :: V2UtilsScripts.Validator
validator = Scripts.validatorScript crowdCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash  validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash

