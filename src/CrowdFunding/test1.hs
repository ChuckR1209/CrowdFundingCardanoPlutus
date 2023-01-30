

correctOutputDatumValue

correctOutputDatumValue :: Bool
correctOutputDatumValue =   case totalValueDatumTxOut of 
                         Just to -> ( to  ==  totalExpectedDatumTxInPlusRedeem)
                         Nothing -> False


totalValueDatumTxOut :: ( Maybe Ledger.Value )
totalValueDatumTxOut = getTotalValueDatum ( datumTokenValue maybeCurrSymTxOut maybeTokenTxOut) targetAmountSoFarValueTxOut ada0
