{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module MintingPolicy where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript, ValidatorHash,
                                       Credential (..), Address (..))
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude     (lengthOfByteString,Bool (..),BuiltinByteString,(==),($),Integer, Maybe (..))
import           Utilities            (validatorHash', wrapValidator, writeValidatorToFile, printDataToJSON)
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

{-# INLINABLE  mkAlwaysFail #-}
mkAlwaysFail :: () -> () -> ScriptContext -> Bool
mkAlwaysFail _dat _red _ctx = False

{-# INLINABLE  mkWrappedAlwaysFail #-}
mkWrappedAlwaysFail :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedAlwaysFail = wrapValidator $ mkAlwaysFail

alwaysFail :: Validator
alwaysFail = mkValidatorScript $$(compile [|| mkWrappedAlwaysFail ||])

saveAlwaysFail :: IO ()
saveAlwaysFail = writeValidatorToFile "assets/AlwaysFail.plutus" alwaysFail

alwaysFailValHash :: ValidatorHash
alwaysFailValHash = validatorHash' alwaysFail

alwaysFailCredential :: Credential 
alwaysFailCredential = ScriptCredential alwaysFailValHash

alwaysFailAddress :: Address
alwaysFailAddress = Address {addressCredential = alwaysFailCredential, addressStakingCredential = Nothing}