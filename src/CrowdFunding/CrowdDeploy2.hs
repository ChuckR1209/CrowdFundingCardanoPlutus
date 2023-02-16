{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CrowdFunding.CrowdDeploy where


import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.ByteString.Short                   as SBS
import qualified Data.ByteString.Base16                  as B16

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                              as DataAeson
import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified PlutusTx
import qualified Ledger
import qualified PlutusTx.Prelude                        as PlutusPrelude 

import qualified CrowdFunding.CrowdFundingOnChain    as OnChain
-- import qualified Helper.GetSlot                                      as GetSlot

--------------------------------------------------------------------
-- Section to insert the Paramemters to initialize the contract
--------------------------------------------------------------------

-- creatorHash :: B.ByteString
-- creatorHash = "80b34df2162e9c4a38ce63322a8f903c9455a0bebd64c02cf1f3222a"




-- parameters :: OnChain.BeneParam
-- parameters = OnChain.BeneParam
--     {
--         OnChain.creator = convertToPubKeyHash creatorHash,
--         OnChain.beneficiary = convertToPubKeyHash beneficiaryHash,
--         OnChain.deadline = paramDeadline
--     }

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
        getTx :: Either String B.ByteString 
        getTx = B16.decode hexBS

convertToPubKeyHash :: B.ByteString -> Ledger.PaymentPubKeyHash
convertToPubKeyHash b = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)

--------------------------------------------------------------------
-- Datum and Redeemer creation
--------------------------------------------------------------------

targetAmount :: Integer
targetAmount = 300000000   -- 300 Ada

dCurrencySymbol :: LedgerApiV2.CurrencySymbol
-- dCurrencySymbol = "88d4e1abfbcd08ace98f41a1a514e84239703c0ab5e5feb61f029eed"
-- dCurrencySymbol = "b7047182a00354f8c4cd7b01c2faab230e01d2f33a6dcfd0c781f7ec"
dCurrencySymbol = "404c7e7ab0e42a8b1a247c2eba252698cad8a19c50c0cf5cfb1c2283" -- last one
dToken    :: LedgerApiV2.TokenName
dToken = "MyCrowdFund"

beneficiaryHash :: B.ByteString
beneficiaryHash = "dbbab47cf610921db8e266c3747cd393db6f9d4b7eb8e348ddeb3971"   -- forPlutus is Beneficiary now

-- 1st contributor is Collateral wallet
contributor1 :: B.ByteString
contributor1 = "0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a"  -- Collateral wallet
   --  "8c573e818f35a8fa8a693933c396561b0622a88bbf34952c4d572cd7"  -- Contributor wallet

-- 2nd contributor is Beneficiary wallet address (not to confuse with Crowd Fund Beneficiary)
contributor2 :: B.ByteString
contributor2 = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"  -- Beneficiary wallet

crowdDeadline :: Ledger.POSIXTime
crowdDeadline = 1676641405000    -- Friday, February 17, 2023 5:43:25 AM GMT-08:00
-- Epoch timestamp: 1676641405
-- Timestamp in milliseconds: 1676641405000
-- Date and time (GMT): Friday, February 17, 2023 1:43:25 PM
-- Date and time (your time zone): Friday, February 17, 2023 5:43:25 AM GMT-08:00



datumCrowd :: OnChain.Dat
datumCrowd = OnChain.Dat { 
                             OnChain.beneficiary = convertToPubKeyHash beneficiaryHash
                           , OnChain.deadline =crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           ,    OnChain.aCurrency = dCurrencySymbol
                           , OnChain.aToken = dToken
                           , OnChain.targetAmount = targetAmount
                           , OnChain.actualtargetAmountsoFar = 2000000
                           , OnChain.contributorsMap = []}

datumCrowdOut :: OnChain.Dat
datumCrowdOut = OnChain.Dat { 
                             OnChain.beneficiary = convertToPubKeyHash beneficiaryHash
                           , OnChain.deadline =crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           ,    OnChain.aCurrency = dCurrencySymbol
                           , OnChain.aToken = dToken
                           , OnChain.targetAmount = targetAmount
                           , OnChain.actualtargetAmountsoFar = 152000000
                           , OnChain.contributorsMap = [(convertToPubKeyHash contributor1,150000000)]}

datumCrowdOut2 :: OnChain.Dat
datumCrowdOut2 = OnChain.Dat { 
                             OnChain.beneficiary = convertToPubKeyHash beneficiaryHash
                           , OnChain.deadline =crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           ,    OnChain.aCurrency = dCurrencySymbol
                           , OnChain.aToken = dToken
                           , OnChain.targetAmount = targetAmount
                           , OnChain.actualtargetAmountsoFar = 302000000
                           , OnChain.contributorsMap = [(convertToPubKeyHash contributor1,150000000), (convertToPubKeyHash contributor2,150000000)]}



-- datumCrowdOut3 :: OnChain.Dat
-- datumCrowdOut3 = OnChain.Dat { 
--                              OnChain.beneficiary = convertToPubKeyHash beneficiaryHash
--                            , OnChain.deadline =crowdDeadline
--     -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
--                            -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
--                            ,    OnChain.aCurrency = dCurrencySymbol
--                            , OnChain.aToken = dToken
--                            , OnChain.targetAmount = targetAmount
--                            , OnChain.actualtargetAmountsoFar = 62000000
--                            , OnChain.contributorsMap = [(convertToPubKeyHash contributor2,60000000)]}



-- Redeemer 



contributorAmount :: Integer
contributorAmount = 150000000   -- 150 Ada contribution from both Contributors

redeemCrowdContribute :: OnChain.Redeem
redeemCrowdContribute = OnChain.Contribute {  
                             OnChain.contribution = (convertToPubKeyHash contributor1, contributorAmount)
                            }

redeemCrowdContributeAsCollateral :: OnChain.Redeem
redeemCrowdContributeAsCollateral = OnChain.Contribute {  
                             OnChain.contribution = (convertToPubKeyHash contributor2, contributorAmount)
                            }

redeemCrowdClose :: OnChain.Redeem
redeemCrowdClose = OnChain.Close


main :: IO()
main = do
    writeDatumUnit
    writeCrowdDatum
    writeCrowdDatumOut
    writeCrowdDatumOut2
    writeCrowdRedeemClose
    writeCrowdRedeemContribute
    writeCrowdRedeemContribute2
    _ <- writeCrowdFunding



    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeDatumUnit :: IO ()
writeDatumUnit = writeJSON "src/CrowdFunding/Deploy/unit.json" ()

writeCrowdDatum :: IO ()
writeCrowdDatum = 
    let crowd = datumCrowd
        d = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFunding-datum.json" d


writeCrowdDatumOut :: IO ()
writeCrowdDatumOut = 
    let crowd = datumCrowdOut
        d = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFunding-datumOut.json" d

writeCrowdDatumOut2 :: IO ()
writeCrowdDatumOut2 = 
    let crowd = datumCrowdOut2
        d = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFunding-datumOut2.json" d

-- writeCrowdDatumOut3 :: IO ()
-- writeCrowdDatumOut3 = 
--     let crowd = datumCrowdOut3
--         d = PlutusTx.toBuiltinData crowd
--     in writeJSON "src/CrowdFunding/Deploy/crowdFunding-datumOut3.json" d

writeCrowdRedeemContribute :: IO ()
writeCrowdRedeemContribute = 
    let crowd = redeemCrowdContribute
        r = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFundingContribute-redeem.json" r


writeCrowdRedeemContribute2 :: IO ()
writeCrowdRedeemContribute2 = 
    let crowd = redeemCrowdContributeAsCollateral
        r = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFundingContribute-redeem2.json" r

writeCrowdRedeemClose :: IO ()
writeCrowdRedeemClose = 
    let crowd = redeemCrowdClose
        r = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFundingClose-redeem.json" r

writeCrowdFunding :: IO (Either (FileError ()) ())
writeCrowdFunding = writeValidator "src/CrowdFunding/Deploy/CrowdFunding.plutus" $ OnChain.validator

