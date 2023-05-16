{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module MintingPolicy where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript)
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude     (lengthOfByteString,Bool (..),BuiltinByteString,(==),($),Integer)
import           Utilities            (wrapPolicy, writeCodeToFile,printDataToJSON)
import           Prelude              (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-VALIDATOR ----------------------------------

type Test0 = BuiltinByteString

data Test1 = ConstructorTest1 BuiltinByteString
makeIsDataIndexed ''Test1 [('ConstructorTest1, 0)]

data Test2 = ConstructorTest2 Integer Integer
makeIsDataIndexed ''Test2 [('ConstructorTest2,0)]

data Test3 = Constructor1Test3 Integer | Constructor2Test3 Integer
makeIsDataIndexed ''Test3 [('Constructor1Test3,0),('Constructor2Test3,1)]

type Test4 = (BuiltinByteString, Test1)

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinByteString -> ScriptContext -> Bool
mkPolicy _red _ctx = True

{-# INLINABLE  mkWrappedPolicy #-}
mkWrappedPolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy $ mkPolicy

policyCode :: CompiledCode (BuiltinData -> BuiltinData -> () )
policyCode = $$(compile [|| mkWrappedPolicy ||])

saveVal :: IO ()
saveVal = writeCodeToFile "./assets/policy.plutus" policyCode

