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
import qualified Plutus.V1.Ledger.Value                                as V1Value
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada
-- import qualified Ledger.Constraints.TxConstraints                as Tx
import qualified Ledger.Value                                      as Value
import qualified Plutus.V2.Ledger.Tx                             as V2LedgerTx
import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Base16                  as B16
import qualified PlutusTx.Prelude                        as PlutusPrelude 

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
        , actualtargetAmountsoFar :: Integer
        -- Keep hash of whole MAP - for now we will do with Map. 
        -- Later need to optimize since if we have 1000s ofcontributors its storage issue
            -- maybe we instead mint an NFT to them instead of storing them here etc. 
        , contributorsMap :: [(Ledger.PaymentPubKeyHash,Integer)] -- Keep hash of whole MAP
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

data Dt1 = Dt1 { 
                   tAmount :: Integer 
      } deriving P.Show




-- 3. onchain code
{-# INLINABLE crowdValidator #-}
crowdValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
crowdValidator d r context =     
    case r of 
      (Contribute cmap ) ->
          --        This validates 3 parameters to be equal 
--           1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - bypasses other Tx-in w/o NFT like Fees Tx-in
--           2nd parameter - Values calculated based on Datum passed to the Validator
--           3rd parametr - Values calculated from Datum at the UTXO.
--           validation#1     
             traceIfFalse "wrong input value" ( correctInputValue d )       -- NEED TO ADD Policy id cannot be blank.

--         ---               ----   traceIfFalse "Only 1 tx-out allowed" correctOutputLength &&   -- not true - can have change address
--         --Only 1 tx-in with datum allowed- other can be payment address fee etc. which dont have datum
--           validation#2
             && traceIfFalse "Only 1 tx-in Datum allowed" only1ValidDatumTxIn 

--           validation#3
--           -- i am expecting only 1 Datum in my tx-out to write back to the Script with NFT - so should not get more than one. Other Tx-out will only have Payment addresses like Change
             && traceIfFalse "Only 1 tx-out Datum allowed"  only1ValidDatumTxOut

--           validation#4
-- --        Validates expected Values based on Datum of tx-out and tx-in - tx-in Value  + redeemer value = tx-out Value
             && traceIfFalse "Constructed Values calculated between tx-out Datum and TxIn datum plus Redeem is wrong" correctOutputDatumValue 

-- --        Validates the actual value at tx-out with calculated Value based on tx-out Datum
             && traceIfFalse "Actual tx-out Values and constructed Datum tx-out dont match" correctOutputValue 

-- --        tx-out - Datum collected amount should be updated with Tx-in amount + contributed amount
             && traceIfFalse "the ContributedSoFar amount has a descrepancy" correctTargetAmountSoFarDatum 

-- --        tx-in and tx-out - Beneficiary, Deadline and Target amount should be same.
             && traceIfFalse "Datums check: Either beneficiary , deadline or targetAmount is not matching" correctRestDatum 

-- --        The tx-out Contributors map should be tx-in Contributor map + Redeem map
             && traceIfFalse "Datums check: Contributors map is not added correctly"  correctContributionMapDatum 

-- --        Scripts address validations for Tx-in and Tx-out
--           traceIfFalse "Scripts address validations for Tx-in and Tx-out fail" addressValidation

-- --        Contributor has to sign
--           && traceIfFalse "Not signed by contributor" (signedByContributor $ fst cmap)


             && traceIfFalse "Redeem amount has to be min 1 Ada" (contributionAmountRedeemer $ snd cmap)

          -- && traceIfFalse "Deadline reached - no more contributions" (not deadlinepassed)
          
      Close -> 
          traceIfFalse "UTXO being spend values are not matching based on Datum" correctInputValueClose
          && traceIfFalse "Target amount not reached" closeTargetAmountValid            
          && traceIfFalse "Not signed by beneficiary" signedByBeneficiary

--         --Only 1 tx-in with datum allowed- other can be payment address fee etc. which dont have datum
          && traceIfFalse "Only 1 tx-in Datum allowed" only1ValidDatumTxIn 
          -- True
    -- -- traceIfFalse "Deadline not yet reached" deadlinepassed
    -- -- need to check that Value being paid to script with NFT does not have any other tokens
    -- -- 

    
    where 
      txinfo :: Contexts.TxInfo 
      txinfo = Contexts.scriptContextTxInfo context
      
      signedByBeneficiary :: Bool
      signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

      signedByContributor :: Ledger.PaymentPubKeyHash -> Bool
      signedByContributor c = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash c

      contributionAmountRedeemer :: Integer -> Bool
      contributionAmountRedeemer i = i >= 1000000
      
      deadlinepassed :: Bool 
      deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)

      inputsAll :: [Contexts.TxInInfo]
      inputsAll = Contexts.txInfoInputs txinfo

      inputsAllResolved :: [Contexts.TxOut]
      inputsAllResolved = fmap Contexts.txInInfoResolved inputsAll  



------------------------------------- Addresses- get only the Script addresses and then validate - only 1 should exist in Tx-in and Tx-out
--    get the addresses from TxOut  - these will be addresses from any Payments and Script address too
      gettxOutAddressFromUTXO :: [Contexts.TxOut] -> [LedgerApiV2.Address]
      gettxOutAddressFromUTXO [] = []
      gettxOutAddressFromUTXO [ad] = [LedgerApiV2.txOutAddress ad]
      gettxOutAddressFromUTXO (ad: ads) = (LedgerApiV2.txOutAddress ad) : (gettxOutAddressFromUTXO  ads)
--    now get the Credentials from the Address
      gettxOutCredentialsFromUTXO :: [LedgerApiV2.Address] -> [LedgerApiV2.Credential]
      gettxOutCredentialsFromUTXO [] = []
      gettxOutCredentialsFromUTXO [cr] = [LedgerApiV2.addressCredential cr]
      gettxOutCredentialsFromUTXO (cr: crs) = (LedgerApiV2.addressCredential cr) : (gettxOutCredentialsFromUTXO  crs)
--    now get the script Credentials from the credentials if its there. Ignore payment addresses 
      gettxOutScriptCredentialsFromUTXO :: [LedgerApiV2.Credential] -> [LedgerApiV2.ValidatorHash]
      gettxOutScriptCredentialsFromUTXO [] = []
      gettxOutScriptCredentialsFromUTXO [(LedgerApiV2.ScriptCredential scr)] = [scr]
      gettxOutScriptCredentialsFromUTXO [_] = []
      gettxOutScriptCredentialsFromUTXO ((LedgerApiV2.ScriptCredential scr) : scrs) = (scr) : (gettxOutScriptCredentialsFromUTXO  scrs)
      gettxOutScriptCredentialsFromUTXO (_ : scrs) = (gettxOutScriptCredentialsFromUTXO  scrs)

      scriptAddressesTxIn :: [LedgerApiV2.ValidatorHash]
      scriptAddressesTxIn = (gettxOutScriptCredentialsFromUTXO (gettxOutCredentialsFromUTXO (gettxOutAddressFromUTXO inputsAllResolved)))

      scriptAddressesTxOut :: [LedgerApiV2.ValidatorHash]
      scriptAddressesTxOut = (gettxOutScriptCredentialsFromUTXO (gettxOutCredentialsFromUTXO (gettxOutAddressFromUTXO outputsAll)))

      addressValidation :: Bool
      addressValidation = ( scriptAddressesTxIn == scriptAddressesTxOut ) && ( (length scriptAddressesTxOut) == 1 )


------------------------------------- Datums
      getTxOutputDatumFromUTXO :: [Contexts.TxOut] -> [V2LedgerTx.OutputDatum]
      getTxOutputDatumFromUTXO [] = []
      getTxOutputDatumFromUTXO [iR] = [V2LedgerTx.txOutDatum iR]
      getTxOutputDatumFromUTXO (iR: iRs) = (V2LedgerTx.txOutDatum iR) : (getTxOutputDatumFromUTXO  iRs)


      getDatumFromUTXO :: [V2LedgerTx.OutputDatum] -> [Ledger.Datum]
      getDatumFromUTXO [] = []
      getDatumFromUTXO [V2LedgerTx.OutputDatum d] = [d]      -- Only take actual Datum and ignore Hash or no Datums
      getDatumFromUTXO [V2LedgerTx.NoOutputDatum] = []
      getDatumFromUTXO [V2LedgerTx.OutputDatumHash hs ] = [] 
      getDatumFromUTXO ((V2LedgerTx.OutputDatum d) : dts ) = d : (getDatumFromUTXO dts)
      getDatumFromUTXO ((_) : dts ) =  (getDatumFromUTXO dts)



      getDatFromUTXODatum :: [Ledger.Datum] -> [Maybe Dat]
      getDatFromUTXODatum [] = [Nothing]
      getDatFromUTXODatum [d] = do 
            case (LedgerApiV2.fromBuiltinData (LedgerApiV2.toBuiltinData d) :: Maybe Dat) of    
              Nothing -> [Nothing]
              dat -> [dat]
      getDatFromUTXODatum _ = [Nothing]



--    Now we get all the elments from Dat - datum and later also validate that this Value exist on UTXO and 
--                    
      getCurSymFromDatum :: [Maybe Dat] -> Maybe LedgerApiV2.CurrencySymbol
      getCurSymFromDatum [(Just d)] = Just (aCurrency d)
      getCurSymFromDatum _ = Nothing

      maybeCurrSymTxIn :: Maybe LedgerApiV2.CurrencySymbol 
      maybeCurrSymTxIn = getCurSymFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))

      maybeCurrSymTxOut :: Maybe LedgerApiV2.CurrencySymbol 
      maybeCurrSymTxOut = getCurSymFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))

      getTokenFromDatum :: [Maybe Dat] -> Maybe LedgerApiV2.TokenName
      getTokenFromDatum [(Just d)] = Just (aToken d)
      getTokenFromDatum _ = Nothing

    --   maybeToken :: Maybe LedgerApiV2.TokenName 
    --   maybeToken = getTokenFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))
      maybeTokenTxIn :: Maybe LedgerApiV2.TokenName 
      maybeTokenTxIn = getTokenFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))

      maybeTokenTxOut :: Maybe LedgerApiV2.TokenName 
      maybeTokenTxOut = getTokenFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))

      getTargetAmountFromDatum :: [Maybe Dat] -> Maybe Integer
      getTargetAmountFromDatum [(Just d)] = Just (targetAmount d)
      getTargetAmountFromDatum _ = Nothing

      maybeTargetAmountTxIn :: Maybe Integer
      maybeTargetAmountTxIn = getTargetAmountFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))

      maybeTargetAmountTxOut :: Maybe Integer
      maybeTargetAmountTxOut = getTargetAmountFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))


------------------------------------- Total Amount so Far
      getActualTargetAmountSoFarFromDatum :: [Maybe Dat] -> Maybe Integer
      getActualTargetAmountSoFarFromDatum [(Just d)] = Just (actualtargetAmountsoFar d)
      getActualTargetAmountSoFarFromDatum _ = Nothing

      maybeTargetAmountSoFarTxIn :: Maybe Integer
      maybeTargetAmountSoFarTxIn = getActualTargetAmountSoFarFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))

      maybeTargetAmountSoFarTxOut :: Maybe Integer
      maybeTargetAmountSoFarTxOut = getActualTargetAmountSoFarFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))





--    Deadline
      getDeadlineFromDatum :: [Maybe Dat] -> Maybe LedgerApiV2.POSIXTime
      getDeadlineFromDatum [(Just d)] = Just (deadline d)
      getDeadlineFromDatum _ = Nothing

      maybeDeadlineFromDatumTxIn :: Maybe LedgerApiV2.POSIXTime
      maybeDeadlineFromDatumTxIn = getDeadlineFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))      

      maybeDeadlineFromDatumTxOut :: Maybe LedgerApiV2.POSIXTime
      maybeDeadlineFromDatumTxOut = getDeadlineFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))      


--    Beneficiary
      getBeneficiaryFromDatum :: [Maybe Dat] -> Maybe Ledger.PaymentPubKeyHash
      getBeneficiaryFromDatum [(Just d)] = Just (beneficiary d)
      getBeneficiaryFromDatum _ = Nothing

      maybeBeneficiaryFromDatumTxIn :: Maybe Ledger.PaymentPubKeyHash
      maybeBeneficiaryFromDatumTxIn = getBeneficiaryFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))  

      maybeBeneficiaryFromDatumTxOut :: Maybe Ledger.PaymentPubKeyHash
      maybeBeneficiaryFromDatumTxOut = getBeneficiaryFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))  


--    Contribution Map
      getContributorsMapFromDatum :: [Maybe Dat] -> Maybe [(Ledger.PaymentPubKeyHash,Integer)]
      getContributorsMapFromDatum [(Just d)] = Just (contributorsMap d)
      getContributorsMapFromDatum _ = Nothing

      maybeContributorsMapFromDatumTxIn :: Maybe [(Ledger.PaymentPubKeyHash,Integer)]
      maybeContributorsMapFromDatumTxIn = getContributorsMapFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved)))

      maybeContributorsMapFromDatumTxOut :: Maybe [(Ledger.PaymentPubKeyHash,Integer)]
      maybeContributorsMapFromDatumTxOut = getContributorsMapFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll)))

      lengthMaybeContributorsMapFromDatumTxIn :: Integer
      lengthMaybeContributorsMapFromDatumTxIn = case maybeContributorsMapFromDatumTxIn of 
                                                  Just x -> length x
                                                  Nothing -> 0
      minAdaToAdd :: LedgerApiV2.Value
      minAdaToAdd = if lengthMaybeContributorsMapFromDatumTxIn == 0
                      then minAda
                      else ada0

      -- Construct the Token from Datum
    --   datumTokenValue :: Maybe Ledger.Value
    --   datumTokenValue = case (maybeCurrSym, maybeToken ) of
    --                       (Just cs, Just tk) ->  Just (Value.singleton (cs) (tk) 1)
    --                       _ -> Nothing
      datumTokenValue :: (Maybe LedgerApiV2.CurrencySymbol) ->  (Maybe LedgerApiV2.TokenName)  -> Maybe Ledger.Value
      datumTokenValue mcs mtn = case (mcs, mtn ) of
                          (Just cs, Just tk) ->  Just (Value.singleton (cs) (tk) 1)
                          _ -> Nothing      
 

      -- Now we need to also add Ada value of how much was already contributed. UTXO value and this UTXO's Datum Ada has to match
      targetAmountSoFarValueTxIn :: Maybe Ledger.Value
      targetAmountSoFarValueTxIn = case (maybeTargetAmountSoFarTxIn) of
                          Just i -> Just (Ada.lovelaceValueOf i)
                          Nothing -> Nothing

      targetAmountSoFarValueTxOut :: Maybe Ledger.Value
      targetAmountSoFarValueTxOut = case (maybeTargetAmountSoFarTxOut) of
                          Just i -> Just (Ada.lovelaceValueOf i)
                          Nothing -> Nothing

      getTotalValueDatum :: (Maybe Ledger.Value) -> ( Maybe Ledger.Value ) -> Ledger.Value  -> ( Maybe Ledger.Value )
      getTotalValueDatum (Just tk)  (Just amt) min = Just (amt <> tk <> min) 
      getTotalValueDatum (Just tk)  (Nothing) min = Just (tk <> min)
      getTotalValueDatum (Nothing)  (Just amt) min = Just (amt <> min) 
      getTotalValueDatum _ _ _ = Nothing


      totalValueDatumTxin :: ( Maybe Ledger.Value )
      totalValueDatumTxin = getTotalValueDatum ( datumTokenValue maybeCurrSymTxIn maybeTokenTxIn) targetAmountSoFarValueTxIn ada0

      totalValueDatumTxOut :: ( Maybe Ledger.Value )
      totalValueDatumTxOut = getTotalValueDatum ( datumTokenValue maybeCurrSymTxOut maybeTokenTxOut) targetAmountSoFarValueTxOut ada0

      -- i am expecting only 1 Datum in my tx-ins - the Spending UTXO - so should not get more than one. Other Tx-ins will only have Payment addresses
      only1ValidDatumTxIn :: Bool
      only1ValidDatumTxIn = length (getDatumFromUTXO (getTxOutputDatumFromUTXO inputsAllResolved )) == 1
      -- only1ValidDatumTxIn = len)gth (getTxOutputDatumFromUTXO inputsAllResolved ) == 1

      -- i am expecting only 1 Datum in my tx-ins - the Spending UTXO - so should not get more than one. Other Tx-ins will only have Payment addresses
      only1ValidDatumTxOut :: Bool
      only1ValidDatumTxOut = length (getDatumFromUTXO (getTxOutputDatumFromUTXO outputsAll )) == 1

--      we need to filter to get only the UTXO with NFT and get values only from that UTXO
        -- getAllDatumValuesTxIns :: [Contexts.TxOut] -> [Contexts.TxOut,  ]
        -- getAllDatumValuesTxIns [] = []
        -- getAllDatumValuesTxIns [txO] = 

--      We cannot get all inputs - cause it will also have Fees, or collateral. '
--      Better to get only the single UTXO with Datum and then take Value of only that UTXO
      getAllValuesTxIns :: [Ledger.Value]
      getAllValuesTxIns = fmap Contexts.txOutValue inputsAllResolved



--    Check correctness of Datum for rest of static fields.
      correctRestDatum :: Bool
      correctRestDatum = (maybeBeneficiaryFromDatumTxIn == maybeBeneficiaryFromDatumTxOut) && (maybeDeadlineFromDatumTxIn == maybeDeadlineFromDatumTxOut) 
                                && (maybeTargetAmountTxIn == maybeTargetAmountTxOut)
      correctContributionMapDatum :: Bool
      correctContributionMapDatum = (maybeContributorsMapFromDatumTxOut == maybeNewContributorDatum)

--    FOr this validation the Target Amount so far has to increase in the tx-out by contribution amount. So we add that from Redeemer.
--    This is only Datum validation. Not value. Thats done with "correctOutputValue"
      correctTargetAmountSoFarDatum :: Bool
      correctTargetAmountSoFarDatum = validateTargetAmountSoFar maybeTargetAmountSoFarTxIn maybeTargetAmountSoFarTxOut


      validateTargetAmountSoFar :: Maybe Integer -> Maybe Integer -> Bool
      validateTargetAmountSoFar (Just x) (Just y) = (x + (snd (contribution r))) == y
      validateTargetAmountSoFar (Just x) Nothing = False
      validateTargetAmountSoFar Nothing (Just y) = False
      validateTargetAmountSoFar Nothing Nothing = False

--    Validate the Contributor map 
--    Add the redeemer to Contributor map from Tx-in Datum contributor map
      maybeNewContributorDatum :: Maybe [(Ledger.PaymentPubKeyHash,Integer)]
      maybeNewContributorDatum = case maybeContributorsMapFromDatumTxIn of 
                                   Just cp -> Just (cp : (contribution r ))
                                   Nothing -> Just [(contribution r ) ]


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
      checkInputFound :: [Ledger.Value] -> Ledger.Value -> (Maybe Ledger.Value) -> Bool
      checkInputFound [] _ (Nothing)= False
      checkInputFound [txV] dtV (Just jdt)= (txV == dtV) && ( dtV == jdt)
      checkInputFound (txV : txs) dtV (Just jdt)= ((txV == dtV) && ( dtV == jdt)) || (checkInputFound txs dtV (Just jdt))
      checkInputFound _ _ _ = False
      -- Get all the Tx-in and get the Values there - so i have my NFT token here along with any other Ada and other Tokens in case.
      --      Then i constructed the tokenValue from Datum that is passed. This is coming from Datum passed to OnChain
      --      but now we also have Datum at the UTXO being consumed - so i actually need to get Datum from there and construct my Value based on orig Datum


--    This validates 3 parameters to be equal 
--             1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - bypasses other Tx-in w/o NFT like Fees Tx-in
--             2nd parameter - Values constructed based on Datum passed to the Validator
--             3rd parametr - Values constructed from Datum at the UTXO. 
      correctInputValue :: Dat -> Bool
      -- correctInputValue dt = checkInputFound getAllValuesTxIns (tokenValue <> Ada.lovelaceValueOf (minLovelace + ( amountInDatum (contributorsMap dt) ))) totalValueDatumTxin
      correctInputValue dt = checkInputFound getAllValuesTxIns (tokenValue <> Ada.lovelaceValueOf  (( amountInDatum (contributorsMap dt) )) <> minAda) (totalValueDatumTxin )  
--         correctInputValue dt = tokenValue == tokenValue <> Ada.lovelaceValueOf (minLovelace + ( amountInDatum (contributorsMap dt) ))    --- Temp
        -- correctInputValue = inVal == tokenValue <> Ada.lovelaceValueOf (minLovelace + amountInDatum) 


      redeemValue :: Ledger.Value
      redeemValue = Ada.lovelaceValueOf (snd (contribution r ))

      totalExpectedDatumTxInPlusRedeem :: Ledger.Value
      totalExpectedDatumTxInPlusRedeem = case totalValueDatumTxin of 
                                      Just ti -> (redeemValue <> ti )
                                      Nothing -> traceError "No txInValue Datum , only redeemValue"

--    This validates value from Datum Tx-in, adds Redeemer to check the Datum tx-out is correct Value point of view.
      correctOutputDatumValue :: Bool
      correctOutputDatumValue =   case totalValueDatumTxOut of 
                               Just to -> ( to  ==  totalExpectedDatumTxInPlusRedeem)
                               Nothing -> False

--    we still need to get the actual Values from Tx out to check aginst the datum of the tx-out (since above step we validated datums point of view and redeemer too)
      checkOutputFound :: [Ledger.Value] -> (Maybe Ledger.Value) -> Bool
      checkOutputFound [] (Nothing)= False
      checkOutputFound [txV] (Just jdt)= (txV == jdt)
      checkOutputFound (txV : txs) (Just jdt)= ((txV ==  jdt)) || (checkOutputFound txs (Just jdt))
      checkOutputFound _ _  = False

      correctOutputValue :: Bool
      correctOutputValue = checkOutputFound getAllValuesTxOut totalValueDatumTxOut

      getAllValuesTxOut :: [Ledger.Value]
      getAllValuesTxOut = fmap Contexts.txOutValue outputsAll

--      Get all the tx-outs
      outputsAll :: [Contexts.TxOut]
      outputsAll = Contexts.txInfoOutputs txinfo



--    Validation of Target amount reached - 
--    1) we are taking the Script UTXO Ada Value (minus the NFT)
--    2) We are taking the TargetAmount from the datum and convert to Ada Value.
--    3) then validate if SCript Value GT then Target Amount.
      -- findOwnInput :: ScriptContext -> Maybe TxInInfo
      scriptTxIn :: Maybe Contexts.TxOut
      scriptTxIn = fmap Contexts.txInInfoResolved (Contexts.findOwnInput context)
      
      scriptValue :: Maybe Ledger.Value
      scriptValue = fmap Contexts.txOutValue scriptTxIn

      adaScriptValue :: Maybe Ada.Ada
      adaScriptValue = fmap Ada.fromValue scriptValue

      onlyAdaScriptValue :: Maybe Ledger.Value
      onlyAdaScriptValue = fmap Ada.toValue adaScriptValue

      maybeTargetAmountScripTxIn :: Maybe Integer
      maybeTargetAmountScripTxIn = case scriptTxIn of 
                                     (Just ti) -> getTargetAmountFromDatum (getDatFromUTXODatum (getDatumFromUTXO (getTxOutputDatumFromUTXO [ti])))
                                     Nothing -> traceError "Txin Own input could not get TargetAmountSoFar"

      targetAmountScript :: Maybe Ada.Ada
      targetAmountScript = fmap Ada.lovelaceOf maybeTargetAmountScripTxIn
      targetAmountAsValue :: Maybe Ledger.Value
      targetAmountAsValue = fmap Ada.toValue targetAmountScript

      closeTargetAmountValid :: Bool
      closeTargetAmountValid = case targetAmountAsValue of 
                                 Just ta -> case onlyAdaScriptValue of 
                                              Just as -> Value.geq as ta
                                              Nothing -> traceError "value geg for target amount failed"
                                 Nothing -> traceError "targetAmountAsValue returned Nothing"
      

      checkCloseInputs :: [Ledger.Value] -> (Maybe Ledger.Value) -> Bool
      checkCloseInputs [] (Nothing)= False
      checkCloseInputs [txV] (Just jdt)= (txV == jdt)
      checkCloseInputs (txV : txs) (Just jdt)= ((txV ==  jdt)) || (checkCloseInputs txs (Just jdt))
      checkCloseInputs _ _  = False
--    This validates 2 parameters to be equal 
--             1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - bypasses other Tx-in w/o NFT like Fees Tx-in
--             2nd parameter - Values constructed based from Datum at the UTXO itself. 
      correctInputValueClose :: Bool
      correctInputValueClose = checkCloseInputs getAllValuesTxIns (totalValueDatumTxin )  


--      Only one output allowed. We spend the UTXO with NFT and pay it back to the SCript with additional contribution updated Datum
--      So there is no other output needed.
      correctOutputLength :: Bool
      correctOutputLength = (length $ (outputsAll)) == 1
      
      --      Get the tx-out Datum(NoOutputDatum is there when there is no datum so every tx-out does have a OutputDatum)
      --      we are only expecting 1 since tx out cant be more than 1. 
      outputDatum :: [V2LedgerTx.OutputDatum]
      outputDatum = fmap Contexts.txOutDatum outputsAll
      
      -- --      get the datum out of the OutputDatum type - we know we have only OutputDatumHash
      -- strippedDatumHash :: [V2LedgerTx.OutputDatum] -> Maybe V2LedgerTx.OutputDatum
      -- strippedDatumHash [] = Nothing
      -- strippedDatumHash [(V2LedgerTx.OutputDatum d)] = Just (V2LedgerTx.OutputDatum d)


--      Here you want to make sure the Token is there and the Ada is greater than what was already there on the UTXO of the input
--              getAllValueTXOuts is what they send on Tx-out.    and the other parameter is from Datum so far.
        -- correctOutputDatum :: Dat -> Bool
        -- correctOutputDatum dt = checkOutputGreater getAllValueTxOuts (tokenValue <> Ada.lovelaceValueOf (minLovelace + ( amountInDatum (contributorsMap dt) )))



-- crowdCompile :: CrowdParam -> Scripts.TypedValidator Crowdf 
-- crowdCompile crowdp = Scripts.mkTypedValidator @Crowdf 
--     ($$(PlutusTx.compile [|| crowdValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode crowdp)
--     $$(PlutusTx.compile [|| wrap ||]) where 
--         wrap = Scripts.mkUntypedValidator @Dat @Redeem







--- --------------------------------------------------TEST CASE BUILD --- 


------------------------------------------------------ first build the data 


-------------------------------------------------- Currency Symbol, Token and Value 
curSym1 :: LedgerApiV2.CurrencySymbol
curSym1 = "6375727253796d6231"   -- Hex of "currSymb1"
curSym2 :: LedgerApiV2.CurrencySymbol
curSym2 = "6375727253796d6232"   -- Hex of "currSymb2"
curSymCrowdFund :: LedgerApiV2.CurrencySymbol
curSymCrowdFund = "43726f776446756e64"   -- Hex of "CrowdFund"
token1 :: LedgerApiV2.TokenName
token1 = "token1"
token2 :: LedgerApiV2.TokenName
token2 = "token2"
crTokenCrowdFund :: LedgerApiV2.TokenName
crTokenCrowdFund = "CrowdFund"

-- singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleTon1 :: LedgerApiV2.Value
singleTon1 = LedgerApiV2.singleton curSym1 token1 ( 1 :: Integer )
singleTon2 :: LedgerApiV2.Value
singleTon2 = LedgerApiV2.singleton curSym2 token2 ( 1 :: Integer )

singleTonCF :: LedgerApiV2.Value
singleTonCF = LedgerApiV2.singleton curSymCrowdFund crTokenCrowdFund ( 1 :: Integer )

singleTonAda0CF :: LedgerApiV2.Value
singleTonAda0CF = singleTonCF <> ada0

ada50b :: LedgerApiV2.Value
ada50b = (Ada.lovelaceValueOf 50)

ada50 :: LedgerApiV2.Value
ada50 = Ada.toValue 50

{-# INLINABLE ada0 #-}
ada0 :: LedgerApiV2.Value
ada0 = Ada.toValue 0

{-# INLINABLE minAda #-}
minAda :: LedgerApiV2.Value
minAda = Ada.toValue 2000000      -- this is LoveLaces

firstNftUtxoValue :: LedgerApiV2.Value
firstNftUtxoValue = minAda <> singleTonCF

firstNftUtxoValue2 :: LedgerApiV2.Value
firstNftUtxoValue2 =  singleTonCF <> minAda
-- test - pluck one Ada from Value of token and Ada
onlyAda :: Ada.Ada
onlyAda = Ada.fromValue firstNftUtxoValue

-- Convert the Ada back to Value.
valueAda :: LedgerApiV2.Value
valueAda = Ada.toValue onlyAda

equalVal :: Bool
equalVal = singleTon1 ==  singleTon2

------------------------------------------------------ DATUM, DATUMHASHes

beneficiaryHash1 ::  B.ByteString
beneficiaryHash1 = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"

beneficiary1 :: Ledger.PaymentPubKeyHash
beneficiary1 = convertToPaymentPubKeyHash beneficiaryHash1

beneficiaryPubKey :: Ledger.PubKeyHash
beneficiaryPubKey = convertToPubKeyHash beneficiaryHash1


contributorHash1 ::  B.ByteString
contributorHash1 = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"

contributor1 = convertToPaymentPubKeyHash contributorHash1

contributorPubKey :: Ledger.PubKeyHash
contributorPubKey = convertToPubKeyHash contributorHash1

cFDatumRaw1 :: Dat
cFDatumRaw1 = Dat 
    {
        beneficiary = beneficiary1
        , deadline = 1671159023000
        , aCurrency = curSymCrowdFund 
        , aToken =crTokenCrowdFund
        , targetAmount = 50000000   -- 50 Ada
        , actualtargetAmountsoFar = 2000000
        , contributorsMap = [] 
    }

cFDatumRaw1Close :: Dat
cFDatumRaw1Close = Dat 
    {
        beneficiary = beneficiary1
        , deadline = 1671159023000
        , aCurrency = curSymCrowdFund 
        , aToken =crTokenCrowdFund
        , targetAmount = 50000000   -- 50 Ada
        , actualtargetAmountsoFar = 52000000
        , contributorsMap = [(contributor1, 50000000)] 
    }

-- Datum Txin for Validation#4 test 1
-- will use cFDatumRaw1 - this is the initial one at the script.
-- cFDatumRawVal4TxIn :: Dat
-- cFDatumRawVal4TxIn = Dat 
--     {
--         beneficiary = beneficiary1
--         , deadline = 1671159023000
--         , aCurrency = curSymCrowdFund 
--         , aToken =crTokenCrowdFund
--         , targetAmount = 50000000   -- 50 Ada
--         , actualtargetAmountsoFar = 2000000    -- 2 Ada already at script
--         , contributorsMap = []   -- no contributor yet
--     }

-- Datum TxOut for Validation#4 test 1 -- 1st contributor adds 30 Ada
cFDatumRawVal4TxOut :: Dat
cFDatumRawVal4TxOut = Dat 
    {
        beneficiary = beneficiary1
        , deadline = 1671159023000
        , aCurrency = curSymCrowdFund 
        , aToken =crTokenCrowdFund
        , targetAmount = 50000000   -- 50 Ada
        , actualtargetAmountsoFar = 32000000    -- 2 Ada + 30 Ada added
        , contributorsMap = [(contributor1, 32000000)]   -- 1st Contributor
    }


cFDatum1BuiltInData :: Dat -> BuiltinData
cFDatum1BuiltInData rd = (PlutusTx.toBuiltinData rd)
-- cFDatum1BuiltInData :: BuiltinData
-- cFDatum1BuiltInData  = (PlutusTx.toBuiltinData cFDatumRaw1)

-- cFdatum1 :: Ledger.Datum
-- cFdatum1 = Ledger.Datum { Ledger.getDatum = cFDatum1BuiltInData}
cFdatum1 :: BuiltinData -> Ledger.Datum
cFdatum1 bd = Ledger.Datum { Ledger.getDatum = bd}

-- datumHash :: Datum -> DatumHash
-- cFdatum1Hash :: Ledger.DatumHash
-- cFdatum1Hash = Ledger.datumHash cFdatum1

-- cfDatumOutputDatum :: V2LedgerTx.OutputDatum
-- cfDatumOutputDatum = V2LedgerTx.OutputDatum  cFdatum1
cfDatumOutputDatum :: Ledger.Datum -> V2LedgerTx.OutputDatum
cfDatumOutputDatum dt = V2LedgerTx.OutputDatum  dt

-- Function - takes Raw Dat and returns OutputDatum
rawDatToOutputDatum :: Dat -> V2LedgerTx.OutputDatum
rawDatToOutputDatum rd = cfDatumOutputDatum (cFdatum1 (cFDatum1BuiltInData rd))

-- cfDatumOutputDatumHash :: V2LedgerTx.OutputDatum
-- cfDatumOutputDatumHash = V2LedgerTx.OutputDatumHash  cFdatum1Hash


datumNoOutputDatum :: V2LedgerTx.OutputDatum
datumNoOutputDatum = V2LedgerTx.NoOutputDatum

dtHs1 :: Ledger.DatumHash
dtHs1 = "43726f776446756e64446174756d48617368"   -- Hex of "CrowdFundDatumHash"
datumOutputDatumHash :: V2LedgerTx.OutputDatum
datumOutputDatumHash = V2LedgerTx.OutputDatumHash dtHs1


      
int1 :: Integer
int1 = 1

dt1 :: Dt1
dt1 = Dt1 { tAmount = int1}

data2 :: PlutusTx.Data 
data2 = PlutusTx.B "abcd"

datum1 :: Ledger.Datum
datum1 = Ledger.Datum { Ledger.getDatum = builtDt1}

builtDt1 :: BuiltinData
builtDt1 = (PlutusTx.dataToBuiltinData data2)
      
-- -- dt1 :: Ledger.Datum
-- -- -- dt1 = Ledger.Datum { Ledger.getDatum = ("dtHs1" :: ByteString)}
-- -- dt1 = Ledger.Datum { Ledger.getDatum = }
datumOutputDatum :: V2LedgerTx.OutputDatum
datumOutputDatum = V2LedgerTx.OutputDatum datum1

datumOutputDatum2 :: V2LedgerTx.OutputDatum
datumOutputDatum2 = V2LedgerTx.OutputDatum (Ledger.Datum {Ledger.getDatum = (PlutusTx.dataToBuiltinData $PlutusTx.B "xyz")})
      

      ------------------------------------------------------------ Redeemer
-- Redeen i guess you can directly send since its not on UTXO to really unscramble
cFRedeemContributeRaw1 :: Redeem
cFRedeemContributeRaw1 = Contribute
    {
        contribution = (contributor1, 2000000)              -- 1st contribution 2 Ada
    }
cFRedeemCloseRaw1 :: Redeem
cFRedeemCloseRaw1 = Close
    

cFRedeemContr1BuiltInData :: BuiltinData
cFRedeemContr1BuiltInData = (PlutusTx.toBuiltinData cFRedeemContributeRaw1)

cFRedeemContributeRaw1Val4 :: Redeem
cFRedeemContributeRaw1Val4 = Contribute
    {
        contribution = (contributor1, 30000000)              -- contribution 30 Ada
    }


      ------------------------------------------------------------ Address related
scripAddr1 :: BuiltinByteString
--addr1 = "43726f776446756e6441646472657373"  -- hex of "CrowdFundAddress"
scripAddr1 = "addr_test1wp02taqyn6rp38g4wqn7h5sxccgwkdzex9cegexxsny4qlczfn2al"    -- CrowdFund Address


payAddr1 :: BuiltinByteString
payAddr1 = "addr_test1vq8f02sr8nhwwckz22zumny59pch3uqmgkjctlgdfk5rs7sx52ldh"    -- Beneficiary Address


addrStkCred :: Maybe Ledger.StakingCredential
addrStkCred = Nothing

scripValHash :: Ledger.ValidatorHash
scripValHash = Ledger.ValidatorHash scripAddr1

payValHash :: Ledger.PubKeyHash
payValHash = Ledger.PubKeyHash payAddr1
-- 0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a

scrCred :: LedgerApiV2.Credential
scrCred = LedgerApiV2.ScriptCredential scripValHash

pubKeyCred :: LedgerApiV2.Credential
pubKeyCred = LedgerApiV2.PubKeyCredential payValHash



crAddress1 :: LedgerApiV2.Address
crAddress1 = LedgerApiV2.Address { 
    LedgerApiV2.addressCredential = scrCred,
    LedgerApiV2.addressStakingCredential = Nothing
}

pyAddress1 :: LedgerApiV2.Address
pyAddress1 = LedgerApiV2.Address { 
    LedgerApiV2.addressCredential = pubKeyCred,
    LedgerApiV2.addressStakingCredential = Nothing
}
      
      -- crTxOutAddress :: LedgerApiV2.Address
      -- crTxOutAddress = addressCredential scrCred
      -- txInInfoResolved1 :: TxOut
      -- txOutAddress txInInfoResolved1 = scrCred
      
      
      
      
      
      ------------------------------------------------------ TX-OUTs
      
      
crTxOut1 :: V2LedgerTx.TxOut
crTxOut1 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = datumOutputDatum2, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = singleTon1 <> singleTon2

}

-- Validation#4
-- this test will have tx-in 2 ada, output Datum 32 Ada - so 1 contributor 
crTxOutVal4 :: V2LedgerTx.TxOut
crTxOutVal4 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRawVal4TxOut), 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = singleTonCF <> ada30 <> minAda

}




      -------- Contributor 1 - tx-out



ada2 :: LedgerApiV2.Value
ada2 = Ada.toValue 2000000

ada30 :: LedgerApiV2.Value
ada30 = Ada.toValue 30000000

cFDatumRawContrC1 :: Dat
cFDatumRawContrC1 = Dat 
    {
        beneficiary = beneficiary1
        , deadline = 1671159023000
        , aCurrency = curSymCrowdFund 
        , aToken =crTokenCrowdFund
        , targetAmount = 50000000   -- 50 Ada
        , actualtargetAmountsoFar = 2000000    -- 2 Ada contribution added to Initial contribution
        , contributorsMap = [(contributor1, 2000000)]    -- -- 2 Ada contribution added to Initial contribution
    }

crTxOutContribution1 :: V2LedgerTx.TxOut
crTxOutContribution1 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRawContrC1),      -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTonCF   <> ada2    -- Actual CrowdFund token expected one.

}

--   setting up the Inital very first UTXO 
-- validation#4 will also use this below one 
crTxOutInitial :: V2LedgerTx.TxOut
crTxOutInitial = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRaw1),  ---  cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTonCF     -- Actual CrowdFund token expected one.

}

crTxOutInitialWrong1 :: V2LedgerTx.TxOut
crTxOutInitialWrong1 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRaw1),          -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTon2     -- Actual CrowdFund token expected one.

}

-- Test with returning the UTXO but with different policy id token 
crTxOutContribution1Diff :: V2LedgerTx.TxOut
crTxOutContribution1Diff = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRawContrC1),      -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTon1 <> ada2    -- Actual CrowdFund token expected one.

}






-- No datum - So this is like payment addr, to test other Tx-in also submitted at the script for Fees or collateral.
crTxOutNoDatum :: V2LedgerTx.TxOut
crTxOutNoDatum = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = pyAddress1,
             V2LedgerTx.txOutDatum  = (V2LedgerTx.NoOutputDatum),          -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTon2    -- Actual CrowdFund token expected one.

}
      

-- this txOut will be used for Tx-in of UTXO for close.  we have enough target amount, min Ada and token.
adaTarget :: LedgerApiV2.Value
adaTarget = Ada.toValue 50000000
crTxOutClose1 :: V2LedgerTx.TxOut
crTxOutClose1 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
             V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRaw1Close),      -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTonCF <> adaTarget    -- Actual CrowdFund token expected one.

}

crTxOutNoDatumClosePaymentTxIn :: V2LedgerTx.TxOut
crTxOutNoDatumClosePaymentTxIn = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = pyAddress1,
             V2LedgerTx.txOutDatum  = (V2LedgerTx.NoOutputDatum),          -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda  -- this is just payment address does not matter.

}

crTxOutNoDatumClosePaymentTxOut :: V2LedgerTx.TxOut
crTxOutNoDatumClosePaymentTxOut = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = pyAddress1,
             V2LedgerTx.txOutDatum  = (V2LedgerTx.NoOutputDatum),          -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
             V2LedgerTx.txOutReferenceScript = Nothing,
             V2LedgerTx.txOutValue = minAda <> singleTonCF  <> adaTarget  -- Tx-out close to Beneficiary,

}



-- -- Validation 4 
-- -- here Txout Datum will have 32Ada -  2 + 30 contribution
-- crTxOutContributionVal4 :: V2LedgerTx.TxOut
-- crTxOutContributionVal4 = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
--              V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRawContrC1),      -- cfDatumOutputDatum,   --  datumOutputDatumHash, 
--              V2LedgerTx.txOutReferenceScript = Nothing,
--              V2LedgerTx.txOutValue = minAda <> singleTonCF   <> ada30    -- Actual CrowdFund token expected one.

-- }

      ------------------------------------------------------ TX-INs
      
-- used also for Validation#4
txIn1 :: Contexts.TxInInfo
txIn1 = Contexts.TxInInfo {
           Contexts.txInInfoOutRef = lTxOutRef,
           Contexts.txInInfoResolved = crTxOutInitial
}

-- this is like a payment addr tx-in - with no Datum
txIn2 :: Contexts.TxInInfo
txIn2 = Contexts.TxInInfo {
           Contexts.txInInfoOutRef = lTxOutRef,
           Contexts.txInInfoResolved = crTxOutNoDatum
}

-- Incorrect Tx-in - Datum is correct but Tx-in has a differnt token
txIn1wrong1 :: Contexts.TxInInfo
txIn1wrong1 = Contexts.TxInInfo {
           Contexts.txInInfoOutRef = lTxOutRef,
           Contexts.txInInfoResolved = crTxOutInitialWrong1
}


-- this is like a payment addr tx-in - with no Datum
txInCloseUTXONFT :: Contexts.TxInInfo
txInCloseUTXONFT = Contexts.TxInInfo {
           Contexts.txInInfoOutRef = lTxOutRef,
           Contexts.txInInfoResolved = crTxOutClose1
}


-- this is like a payment addr tx-in - with no Datum
txInClosePayment :: Contexts.TxInInfo
txInClosePayment = Contexts.TxInInfo {
           Contexts.txInInfoOutRef = lTxOutRef,
           Contexts.txInInfoResolved = crTxOutNoDatumClosePaymentTxIn
}


lTxOutRef :: Contexts.TxOutRef
lTxOutRef = Contexts.TxOutRef {
    Contexts.txOutRefId = lId,
      Contexts.txOutRefIdx = (1 :: Integer)
}
lId :: V2LedgerTx.TxId 
lId = V2LedgerTx.TxId {
    V2LedgerTx.getTxId = lGetTxId
}

lGetTxId ::  BuiltinByteString
lGetTxId = "sampleTestTxId"
      
--TxInfo structure
--     txInfoInputs :: [TxInInfo]
--           txInInfoOutRef :: TxOutRef
                   -- txOutRefId :: TxId
                         -- getTxId :: BuiltinByteString
                   -- txOutRefIdx :: Integer
--           txInInfoResolved :: TxOut
                   -- txOutAddress :: Address
                          -- addressCredential :: Credential
                                 -- PubKeyCredential PubKeyHash
                                     -- getPubKeyHash :: BuiltinByteString
                                 -- ScriptCredential ValidatorHash
                                     -- ValidatorHash BuiltinByteString
                          -- addressStakingCredential :: Maybe StakingCredential  (can do Nothing here)
                   -- txOutValue :: Value
                        -- getValue :: Map CurrencySymbol (Map TokenName Integer)
                   -- txOutDatum :: OutputDatum
                        -- NoOutputDatum
                        -- OutputDatumHash DatumHash
                               -- DatumHash BuiltinByteString
                        -- OutputDatum Datum
                               -- getDatum :: BuiltinData
                   -- txOutReferenceScript :: Maybe ScriptHash   -- can go with Nothing here.

--     txInfoOutputs :: [TxOut]



      ------------------------------------------------------ Context & TxInfo

-- data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
--     deriving stock (Generic, Haskell.Eq, Haskell.Show)

crSCriptContext1 :: Contexts.ScriptContext
crSCriptContext1 = Contexts.ScriptContext {
    Contexts.scriptContextTxInfo = crTxInfo1,
    Contexts.scriptContextPurpose = Contexts.Spending lTxOutRef
}

crTxInfo1 = Contexts.TxInfo {
              -- Contexts.txInfoInputs  = [txIn1]           -- ^ Transaction inputs
              Contexts.txInfoInputs  = [txIn1, txIn2]    -- 2 TxInfoInputs, 1 is UTXO with NFT, and other just Payment addr for Fee or collaterals etc
              -- Contexts.txInfoInputs  = [txIn1wrong1]     -- Incorrect test with Correct Datum but Tx-in has different token than the Datum
--     , Contexts.txInfoReferenceInputs :: [TxInInfo]    -- ^ Transaction reference inputs
            -- , Contexts.txInfoOutputs = [crTxOutContribution1]   -- Contribution 1 
             , Contexts.txInfoOutputs = [crTxOutContribution1, crTxOutNoDatum]   -- Contribution 1 + any other tx-out payment address like Change address ex.
            -- , Contexts.txInfoOutputs = [crTxOutContribution1Diff, crTxOutNoDatum]   -- False test- sending a different NFT token instead of our CrowdFund
            -- , Contexts.txInfoOutputs = [crTxOut1]        
            -- , Contexts.txInfoOutputs = [crTxOut1, crTxOut1]        --    Incorrect test1 - more than 1 outputs
--     , Contexts.txInfoFee             :: Value -- ^ The fee paid by this transaction.
--     , Contexts.txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
--     , Contexts.txInfoDCert     = []      :: [DCert] -- ^ Digests of certificates included in this transaction
--     , Contexts.txInfoWdrl            :: Map StakingCredential Integer -- ^ Withdrawals
--     , Contexts.txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
       --, Contexts.txInfoSignatories = [contributorPubKey]  --    :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
       , Contexts.txInfoSignatories = [beneficiaryPubKey]
--     , Contexts.txInfoRedeemers       :: Map ScriptPurpose Redeemer
--     , Contexts.txInfoData            :: Map DatumHash Datum
--     , Contexts.txInfoId              :: TxId
--     -- ^ Hash of the pending transaction (excluding witnesses)
}

-- data TxInfo = TxInfo
--     { txInfoInputs          :: [TxInInfo] -- ^ Transaction inputs
--     , txInfoReferenceInputs :: [TxInInfo] -- ^ Transaction reference inputs
--     , txInfoOutputs         :: [TxOut] -- ^ Transaction outputs
--     , txInfoFee             :: Value -- ^ The fee paid by this transaction.
--     , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
--     , txInfoDCert           :: [DCert] -- ^ Digests of certificates included in this transaction
--     , txInfoWdrl            :: Map StakingCredential Integer -- ^ Withdrawals
--     , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
--     , txInfoSignatories     :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
--     , txInfoRedeemers       :: Map ScriptPurpose Redeemer
--     , txInfoData            :: Map DatumHash Datum
--     , txInfoId              :: TxId
--     -- ^ Hash of the pending transaction (excluding witnesses)
--     } deriving stock (Generic, Haskell.Show, Haskell.Eq)


crSCriptContextClose :: Contexts.ScriptContext
crSCriptContextClose = Contexts.ScriptContext {
    Contexts.scriptContextTxInfo = crTxInfoClose,
    Contexts.scriptContextPurpose = Contexts.Spending lTxOutRef
}

crTxInfoClose = Contexts.TxInfo {
              Contexts.txInfoInputs  = [txInCloseUTXONFT, txInClosePayment]    -- 2 TxInfoInputs, 1 is UTXO with NFT, and other just Payment addr for Fee or collaterals etc
             , Contexts.txInfoOutputs = [crTxOutNoDatumClosePaymentTxOut]   -- Close out Tx-out
             , Contexts.txInfoSignatories = [beneficiaryPubKey]

}




--------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------
-- crowdValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
test1Bool = crowdValidator cFDatumRaw1 cFRedeemContributeRaw1 crSCriptContext1

-- Close test
test2BoolClose = crowdValidator cFDatumRaw1Close cFRedeemCloseRaw1 crSCriptContextClose
--------------------------------------------------------------------
-- Some Helper functions
--------------------------------------------------------------------
decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBS =
    case getTx of
        Right decHex -> do
            PlutusPrelude.toBuiltin(decHex)  
        Left _ -> do
            PlutusPrelude.emptyByteString 
    where
        getTx :: Either P.String B.ByteString 
        getTx = B16.decode hexBS

convertToPaymentPubKeyHash :: B.ByteString -> Ledger.PaymentPubKeyHash
convertToPaymentPubKeyHash b = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)

convertToPubKeyHash :: B.ByteString -> Ledger.PubKeyHash
convertToPubKeyHash b = Ledger.PubKeyHash $ decodeHex b





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



-------------------------------------------------------------TEMP TESTS


-------------- signators tests

l_crowdValidatorSign :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
l_crowdValidatorSign d (Contribute cmap) context =  
                                (l_signedByContributor $ fst cmap)
l_test3BoolSign = l_crowdValidatorSign cFDatumRaw1 cFRedeemContributeRaw1 crSCriptContext1

l_signedByContributor :: Ledger.PaymentPubKeyHash -> Bool
l_signedByContributor c = Contexts.txSignedBy l_txinfoSign $ Ledger.unPaymentPubKeyHash c

l_txinfoSign :: Contexts.TxInfo 
l_txinfoSign = Contexts.scriptContextTxInfo crSCriptContext1

l_inputsAllSign :: [Contexts.TxInInfo]
l_inputsAllSign = Contexts.txInfoInputs l_txinfoSign

l_inputsAllResolvedSign :: [Contexts.TxOut]
l_inputsAllResolvedSign = fmap Contexts.txInInfoResolved l_inputsAllSign  


-------------------------------------------------------------------
l_gettxOutAddressFromUTXO :: [Contexts.TxOut] -> [LedgerApiV2.Address]
l_gettxOutAddressFromUTXO [] = []
l_gettxOutAddressFromUTXO [ad] = [LedgerApiV2.txOutAddress ad]
l_gettxOutAddressFromUTXO (ad: ads) = (LedgerApiV2.txOutAddress ad) : (l_gettxOutAddressFromUTXO  ads)
--  l_now get the Credentials from the Address
l_gettxOutCredentialsFromUTXO :: [LedgerApiV2.Address] -> [LedgerApiV2.Credential]
l_gettxOutCredentialsFromUTXO [] = []
l_gettxOutCredentialsFromUTXO [cr] = [LedgerApiV2.addressCredential cr]
l_gettxOutCredentialsFromUTXO (cr: crs) = (LedgerApiV2.addressCredential cr) : (l_gettxOutCredentialsFromUTXO  crs)
--  l_now get the script Credentials from the credentials if its there. Ignore payment addresses 
l_gettxOutScriptCredentialsFromUTXO :: [LedgerApiV2.Credential] -> [LedgerApiV2.ValidatorHash]
l_gettxOutScriptCredentialsFromUTXO [] = []
l_gettxOutScriptCredentialsFromUTXO [(LedgerApiV2.ScriptCredential scr)] = [scr]
l_gettxOutScriptCredentialsFromUTXO [_] = []
l_gettxOutScriptCredentialsFromUTXO ((LedgerApiV2.ScriptCredential scr) : scrs) = (scr) : (l_gettxOutScriptCredentialsFromUTXO  scrs)
l_gettxOutScriptCredentialsFromUTXO (_ : scrs) = (l_gettxOutScriptCredentialsFromUTXO  scrs)

l_scriptAddressesTxIn :: [LedgerApiV2.ValidatorHash]
l_scriptAddressesTxIn = (l_gettxOutScriptCredentialsFromUTXO (l_gettxOutCredentialsFromUTXO (l_gettxOutAddressFromUTXO l_inputsAllResolved)))
l_scriptAddressesTxOut :: [LedgerApiV2.ValidatorHash]
l_scriptAddressesTxOut = (l_gettxOutScriptCredentialsFromUTXO (l_gettxOutCredentialsFromUTXO (l_gettxOutAddressFromUTXO ([crTxOutContribution1, crTxOutNoDatum]))))

l_inputsAllResolved :: [Contexts.TxOut]
l_inputsAllResolved = fmap Contexts.txInInfoResolved ([txIn1, txIn2])




-- close testing


l_crowdValidator :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
l_crowdValidator d r context =    
          l_correctInputValueClose
          -- traceIfFalse "Target amount not reached" closeTargetAmountValid    

l_test2BoolClose = l_crowdValidator cFDatumRaw1Close cFRedeemCloseRaw1 crSCriptContextClose

l_txinfoClose :: Contexts.TxInfo 
l_txinfoClose = Contexts.scriptContextTxInfo crSCriptContextClose

l_inputsAllClose :: [Contexts.TxInInfo]
l_inputsAllClose = Contexts.txInfoInputs l_txinfoClose

l_inputsAllResolvedClose :: [Contexts.TxOut]
l_inputsAllResolvedClose = fmap Contexts.txInInfoResolved l_inputsAllClose  


l_correctInputValueClose :: Bool
l_correctInputValueClose = l_checkCloseInputs l_getAllValuesTxIns (l_totalValueDatumTxin l_inputsAllResolvedClose) 


l_checkCloseInputs :: [Ledger.Value] -> (Maybe Ledger.Value) -> Bool
l_checkCloseInputs [] (Nothing) = False
l_checkCloseInputs [txV] (Just jdt)= (txV == jdt)
l_checkCloseInputs (txV : txs) (Just jdt)= ((txV ==  jdt)) || (l_checkCloseInputs txs (Just jdt))
l_checkCloseInputs _ _  = False


l_getAllValuesTxIns :: [Ledger.Value]
l_getAllValuesTxIns = fmap Contexts.txOutValue l_inputsAllResolvedClose


l_lengthMaybeContributorsMapFromDatumTxIn :: Integer
l_lengthMaybeContributorsMapFromDatumTxIn = case l_maybeContributorsMapFromDatumTxIn of 
                                                  Just x -> length x
                                                  Nothing -> 0


l_maybeContributorsMapFromDatumTxIn :: Maybe [(Ledger.PaymentPubKeyHash,Integer)]
l_maybeContributorsMapFromDatumTxIn = l_getContributorsMapFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO l_inputsAllResolvedClose)))

l_getContributorsMapFromDatum :: [Maybe Dat] -> Maybe [(Ledger.PaymentPubKeyHash,Integer)]
l_getContributorsMapFromDatum [(Just d)] = Just (contributorsMap d)
l_getContributorsMapFromDatum _ = Nothing


minAdaToAdd :: LedgerApiV2.Value
minAdaToAdd = if l_lengthMaybeContributorsMapFromDatumTxIn == 0
                then minAda
                else ada0

l_totalValueDatumTxin :: [Contexts.TxOut] -> ( Maybe Ledger.Value )
l_totalValueDatumTxin txIn = l_getTotalValueDatum ( l_datumTokenValue l_maybeCurrSymTxIn l_maybeTokenTxIn) (l_targetAmountSoFarValueTxIn txIn) ada0

l_targetAmountSoFarValueTxIn :: [Contexts.TxOut] -> Maybe Ledger.Value
l_targetAmountSoFarValueTxIn txIn = case (l_maybeTargetAmountSoFarTxIn txIn) of
                          Just i -> Just (Ada.lovelaceValueOf i)
                          Nothing -> Nothing

l_maybeTargetAmountSoFarTxIn :: [Contexts.TxOut] -> Maybe Integer
l_maybeTargetAmountSoFarTxIn txIn = l_getActualTargetAmountSoFarFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO txIn)))

l_getTxOutputDatumFromUTXO :: [Contexts.TxOut] -> [V2LedgerTx.OutputDatum]
l_getTxOutputDatumFromUTXO [] = []
l_getTxOutputDatumFromUTXO [iR] = [V2LedgerTx.txOutDatum iR]
l_getTxOutputDatumFromUTXO (iR: iRs) = (V2LedgerTx.txOutDatum iR) : (l_getTxOutputDatumFromUTXO  iRs)

l_getDatumFromUTXO :: [V2LedgerTx.OutputDatum] -> [Ledger.Datum]
l_getDatumFromUTXO [] = []
l_getDatumFromUTXO [V2LedgerTx.OutputDatum d] = [d]      -- Only take actual Datum and ignore Hash or no Datums
l_getDatumFromUTXO [V2LedgerTx.NoOutputDatum] = []
l_getDatumFromUTXO [V2LedgerTx.OutputDatumHash hs ] = [] 
l_getDatumFromUTXO ((V2LedgerTx.OutputDatum d) : dts ) = d : (l_getDatumFromUTXO dts)

l_getDatFromUTXODatum :: [Ledger.Datum] -> [Maybe Dat]
l_getDatFromUTXODatum [] = [Nothing]
l_getDatFromUTXODatum [d] = do 
      case (LedgerApiV2.fromBuiltinData (LedgerApiV2.toBuiltinData d) :: Maybe Dat) of    
        Nothing -> [Nothing]
        dat -> [dat]
l_getDatFromUTXODatum _ = [Nothing]

l_getActualTargetAmountSoFarFromDatum :: [Maybe Dat] -> Maybe Integer
l_getActualTargetAmountSoFarFromDatum [(Just d)] = Just (actualtargetAmountsoFar d)
l_getActualTargetAmountSoFarFromDatum _ = Nothing

l_maybeTokenTxIn :: Maybe LedgerApiV2.TokenName 
l_maybeTokenTxIn = l_getTokenFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO l_inputsAllResolvedClose)))

l_maybeCurrSymTxIn :: Maybe LedgerApiV2.CurrencySymbol 
l_maybeCurrSymTxIn = l_getCurSymFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO l_inputsAllResolvedClose)))

l_getTotalValueDatum :: (Maybe Ledger.Value) -> ( Maybe Ledger.Value ) -> Ledger.Value  -> ( Maybe Ledger.Value )
l_getTotalValueDatum (Just tk)  (Just amt) min = Just (tk <> amt <> min) 
l_getTotalValueDatum (Just tk)  (Nothing) min = Just (tk <> min)
l_getTotalValueDatum (Nothing)  (Just amt) min = Just (amt <> min) 
l_getTotalValueDatum _ _ _ = Nothing

l_datumTokenValue :: (Maybe LedgerApiV2.CurrencySymbol) ->  (Maybe LedgerApiV2.TokenName)  -> Maybe Ledger.Value
l_datumTokenValue mcs mtn = case (mcs, mtn ) of
                          (Just cs, Just tk) ->  Just (Value.singleton (cs) (tk) 1)
                          _ -> Nothing   

l_getTokenFromDatum :: [Maybe Dat] -> Maybe LedgerApiV2.TokenName
l_getTokenFromDatum [(Just d)] = Just (aToken d)
l_getTokenFromDatum _ = Nothing

l_getCurSymFromDatum :: [Maybe Dat] -> Maybe LedgerApiV2.CurrencySymbol
l_getCurSymFromDatum [(Just d)] = Just (aCurrency d)
l_getCurSymFromDatum _ = Nothing




-- Validation#4 test 1
----- for correctOutputDatumValue validation

-- create the context for valiation 4 -
-- has Tx-in - initial input with 2 ada in txIn1
-- had Tx-out with crTxOutVal4

-- Step 1 - Build your context
-- Step 2 - Build the Tx-ins and Tx-out, signatories if needed
-- Step 3 - Build your DatumTxIn and DatumTxOut, and Datum to be passed, and Redeemer
-- 


lScriptContextVal4 :: Contexts.ScriptContext
lScriptContextVal4 = Contexts.ScriptContext {
    Contexts.scriptContextTxInfo = l_ContextVal4,
    Contexts.scriptContextPurpose = Contexts.Spending lTxOutRef
}

l_ContextVal4 :: Contexts.TxInfo
l_ContextVal4 = Contexts.TxInfo {
              Contexts.txInfoInputs  = [txIn1, txIn2]    -- 2 TxInfoInputs, 1 is UTXO with NFT, and other just Payment addr for Fee or collaterals etc
             , Contexts.txInfoOutputs = [crTxOutVal4, crTxOutNoDatum]   -- Contribution 1 + any other tx-out payment address like Change address ex.
             , Contexts.txInfoSignatories = [beneficiaryPubKey]

}

--      Get all the tx-outs
l_outputsAllVal4 :: [Contexts.TxOut]
l_outputsAllVal4 = Contexts.txInfoOutputs l_ContextVal4

l_txinfoVal4 :: Contexts.TxInfo 
l_txinfoVal4 = Contexts.scriptContextTxInfo lScriptContextVal4

l_inputsAllVal4 :: [Contexts.TxInInfo]
l_inputsAllVal4 = Contexts.txInfoInputs l_txinfoVal4

l_inputsAllResolvedVal4 :: [Contexts.TxOut]
l_inputsAllResolvedVal4 = fmap Contexts.txInInfoResolved l_inputsAllVal4  





-- cFRedeemContributeRaw1 --- need to set the value for this test.
-- l_outputsAll -- need to set value for this test

-- --        Validates expected Values based on Datum of tx-out and tx-in - tx-in Value  + redeemer value = tx-out Value
l_correctOutputDatumValue :: Bool
l_correctOutputDatumValue =   case (l_totalValueDatumTxOut l_outputsAllVal4) of 
                         Just to -> ( to  ==  l_totalExpectedDatumTxInPlusRedeem l_inputsAllResolvedVal4)
                         Nothing -> False

l_totalExpectedDatumTxInPlusRedeem :: [Contexts.TxOut]  -> Ledger.Value
l_totalExpectedDatumTxInPlusRedeem txIn = case (l_totalValueDatumTxin txIn) of 
                                Just ti -> (l_redeemValue <> ti )
                                Nothing -> traceError "No txInValue Datum , only redeemValue"

l_redeemValue :: Ledger.Value
l_redeemValue = Ada.lovelaceValueOf (snd (contribution cFRedeemContributeRaw1Val4 ))

l_totalValueDatumTxOut :: [Contexts.TxOut] -> ( Maybe Ledger.Value )
l_totalValueDatumTxOut txOut = l_getTotalValueDatum ( l_datumTokenValue l_maybeCurrSymTxOut l_maybeTokenTxOut) (l_targetAmountSoFarValueTxOut txOut) ada0


l_targetAmountSoFarValueTxOut :: [Contexts.TxOut] -> Maybe Ledger.Value
l_targetAmountSoFarValueTxOut txOut = case (l_maybeTargetAmountSoFarTxOut txOut) of
                    Just i -> Just (Ada.lovelaceValueOf i)
                    Nothing -> Nothing

l_maybeTargetAmountSoFarTxOut :: [Contexts.TxOut] -> Maybe Integer
l_maybeTargetAmountSoFarTxOut txOut = l_getActualTargetAmountSoFarFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO txOut)))





l_maybeTokenTxOut :: Maybe LedgerApiV2.TokenName 
l_maybeTokenTxOut = l_getTokenFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO l_outputsAllVal4)))

l_maybeCurrSymTxOut :: Maybe LedgerApiV2.CurrencySymbol 
l_maybeCurrSymTxOut = l_getCurSymFromDatum (l_getDatFromUTXODatum (l_getDatumFromUTXO (l_getTxOutputDatumFromUTXO l_outputsAllVal4)))



-- Breakdowns
-- CONTIBUTION
--    tx-out -- only 1 allowed with Datum - which is to write back to script address with NFT
--       we need to take the Value at tx-out with Datum and it should be Value at UTXO spent + Contribution Amount (this amount comes from Redeem action )
--       we need to also get the Datum being submitted with this Tx-out and make its same as earlier datum with below changes:
--               Datum Change 1 = amount so contributed -- this needs to add contributors amount
--               Datum Change 2 = Map of contributor pubkey hash, will only add this new one. (so we take TX-in Datum to check agains this Tx-out Datum)
--               Rest remain the same - beneficiary, deadline, curr sym, token, Target Amount

-- TO DO 
-- 1) later need to also test Own address from tx-out to Context address --- this will make sure the tx-out is writing back to the script.
-- DONE 2) I also need to take the UTXO being spent Value (i.e Tx-in), and add the contribution to check Value from the tx-out. since we said only 1 tx-out its a bit easier.
-- 3) later look at the spendFromScript.sh and see anything else is there.
-- DONE 4) Tx-out also can be more than 1 since there is Change from Fee. so only validate tx-outs can have only 1 with Datum. So need to validate this.
-- DONE 5) Need to validate Tx-in Datum ContributedSoFar + Redeem value = Tx-out Datum ContributedSoFar.
-- 6) Need to validate the Tx-in Datum ContributedMap + Redeem contributor value = Tx-out Datum ContributedMap correctly
-- 7) Need to valiate rest equal fields of Datum Tx-in and Datum tx-out
-- 8) Need to add Deadline stuff too
-- 9) Need to code for Close too.
-- 10) Need to add Redeem Contributer has to sign.     --- test Failed when i sent Beneficiary where contributor need to sign
-- 11) Of course Redeem Contributor amount is > 0 too
-- 12) Out side where whatever you use Inline them pragma... 

-- Testing to do done
-- DONE -- 1) Need to test 2 Tx-ins and only 1 with Datum and other just like a payment address so no datum.
                    --               Contexts.txInfoInputs  = [txIn1, txIn2]    -- 2 TxInfoInputs, 1 is UTXO with NFT, and other just Payment addr for Fee or collaterals etc
-- DONE - 2) Only 1 tx-out with Datum , rest can be payment address like change. 
-- DONE   3) validated the input Tx-in UTXO with NFT and any value, to match its own Datum NFT + value and Datum called by Contributor - all 3 to match. 
--        need couple more regression tests for this triple case.
--        4) Need to tx-out Breakdown testing from above
-- DONE   5) tx-out to a diff Token than our CrowdFund
--                    -- , Contexts.txInfoOutputs = [crTxOutContribution1Diff, crTxOutNoDatum]   -- False test- sending a different NFT token instead of our CrowdFund