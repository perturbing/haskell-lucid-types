# Content
This repository contains examples on how to switch from Haskell types to Lucid types
# Usage
To use enter a shell via
```
nix develop github:input-output-hk/devX#ghc8107-iog
```
After a cabal update enter a repl via
```
cabal repl Plutus
```
In this repl, import and set the following
```
import MintingPolicy 
import Utilities
import PlutusTx.Prelude
:set -XOverloadedStrings
```
In this repl, you can construct examples described in the file `lucid/offchain.ts'. As an example, one can try in the repl test 2
```
> a = ConstructorTest2 10 11 :: Test2
> printDataToJSON a
{
    "constructor": 0,
    "fields": [
        {
            "int": 10
        },
        {
            "int": 11
        }
    ]
}
```
In the lucid file you can see how this type would translate to the offchain code.