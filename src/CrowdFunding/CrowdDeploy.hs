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
targetAmount = 5000000000   -- 5000 Ada

beneficiaryHash :: B.ByteString
beneficiaryHash = "dbbab47cf610921db8e266c3747cd393db6f9d4b7eb8e348ddeb3971"  -- forPlutus wallet


contributorHash :: B.ByteString
contributorHash = "8c573e818f35a8fa8a693933c396561b0622a88bbf34952c4d572cd7"  -- Contributor wallet

crowdDeadline :: Ledger.POSIXTime
crowdDeadline = 1671159023000

datumCrowd :: OnChain.Dat
datumCrowd = OnChain.Dat { 
                             OnChain.beneficiary = convertToPubKeyHash beneficiaryHash
                           , OnChain.deadline =crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           ,    OnChain.aCurrency = "29274fbae90423d1244e88257b685c9d2fefef4b50e75029e1a10562"
                           , OnChain.aToken = "MyCrowdFund"
                           , OnChain.targetAmount = targetAmount
                           , OnChain.contributorsMap = []}



-- Redeemer 



contributorAmount :: Integer
contributorAmount = 2000000   -- 2 Ada contribution

redeemCrowdContribute :: OnChain.Redeem
redeemCrowdContribute = OnChain.Contribute {  
                             OnChain.contribution = (convertToPubKeyHash contributorHash, contributorAmount)
                            }

redeemCrowdClose :: OnChain.Redeem
redeemCrowdClose = OnChain.Close


main :: IO()
main = do
    writeDatumUnit
    writeCrowdDatum
    writeCrowdRedeemClose
    writeCrowdRedeemContribute
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


writeCrowdRedeemContribute :: IO ()
writeCrowdRedeemContribute = 
    let crowd = redeemCrowdContribute
        r = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFundingContribute-redeem.json" r

writeCrowdRedeemClose :: IO ()
writeCrowdRedeemClose = 
    let crowd = redeemCrowdClose
        r = PlutusTx.toBuiltinData crowd
    in writeJSON "src/CrowdFunding/Deploy/crowdFundingClose-redeem.json" r

writeCrowdFunding :: IO (Either (FileError ()) ())
writeCrowdFunding = writeValidator "src/CrowdFunding/Deploy/CrowdFunding.plutus" $ OnChain.validator

