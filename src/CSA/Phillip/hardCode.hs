--- These magic numbers for the hardcoded PubKeyHash can be found by using  [indexByteString (getPubKeyHash p) i | i <- [0..27]] where p is the PubKeyHash in this case let (p :: PubKeyHash) = "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"
manager :: PubKeyHash
manager = PubKeyHash { getPubKeyHash = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167]) }

--manage2 = PubKeyHash { getPubKeyHash =  (consByteString (consByteString emptyByteString 167) 60) ...  [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167] }

{-# INLINABLE anaCurrencySymbol #-}
--- These magic numbers for the hardcoded CurrencySymbol can be found by using  [indexByteString (unCurrencySymbol c) i | i <- [0..27]] where c is the CurrencySymbol in this case let (c :: CurrencySymbol) = "policyIdHere"
anaCurrencySymbol :: CurrencySymbol
anaCurrencySymbol = CurrencySymbol { unCurrencySymbol = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [97,110,97]) }

{-# INLINABLE anaTokenName #-}
--- These magic numbers for the hardcoded TokenName can be found by using  [indexByteString (unTokenName tokN) i | i <- [0..( (-) (lengthOfByteString (unTokenName tokN)) 1)]] where tokN is the TokenName
--- in this case let (tokN :: TokenName) = "tokenNameHere"
anaTokenName :: TokenName 
anaTokenName = TokenName { unTokenName = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [65,110,97,115,116,97,115,105,97]) }