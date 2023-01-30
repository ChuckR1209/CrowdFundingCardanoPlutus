-- 1. dependecies
--{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
--{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
-- {-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 

-- 2. imports external/imports

module CrowdFunding.CrowdFundingOnChain where


import qualified Prelude                                            as P
import Data.Maybe                                                   (fromJust)
import PlutusTx.Prelude                   
import qualified Ledger                                             as Ledger
import qualified Ledger.Ada                                         as Ada
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Contexts                          as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value                             as LedgerValueV1
import qualified Plutus.V2.Ledger.Api                               as LedgerApiV2
import qualified Plutus.V2.Ledger.Tx                                as PlutusV2LedgerTx
import qualified Plutus.V1.Ledger.Scripts                           as ScriptsLedger
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators    as V2UtilsTypeScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators    as V1UtilsTypeScripts
import qualified Plutus.Script.Utils.Typed                          as UtilsTypeScripts
import qualified Ledger.Typed.Scripts as Scripts


-- import qualified Prelude                                as P
-- import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
-- import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
-- import qualified Plutus.V2.Ledger.Contexts                       as Contexts
-- import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
-- import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
-- import qualified Plutus.V1.Ledger.Value                                as V1Value
-- import qualified Ledger                                          
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import qualified Ledger.Ada                                      as Ada
-- -- import qualified Ledger.Constraints.TxConstraints                as Tx
-- import qualified Ledger.Value                                      as Value
-- import qualified Plutus.V2.Ledger.Tx                             as V2LedgerTx
-- import qualified Data.ByteString.Char8                   as B
-- import qualified Data.ByteString.Base16                  as B16
-- import qualified PlutusTx.Prelude                        as PlutusPrelude 

data RedeemContractor = StartProject |
                        WithdrawPermitPayment | 
                        WithdrawRoughPayment | 
                        WithdrawDrywallPayment | 
                        WithdrawFinalPayment

PlutusTx.unstableMakeIsData ''RedeemContractor
PlutusTx.makeLift ''RedeemContractor

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


{-# INLINABLE milestoneValidator #-}
milestoneValidator :: Dat -> LedgerContextsV2.ScriptContext -> Bool
milestoneValidator dat sc =
    let       
      datumActionMatchesRedeem :: RedeemContractor -> Bool
      datumActionMatchesRedeem rc = True
    in 
      traceError "error"
