import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as mod from "https://deno.land/std@0.182.0/crypto/mod.ts";
import {decode} from "https://deno.land/std/encoding/hex.ts";

const lucid = await L.Lucid.new(
  undefined,
  "Preview"
);

// cardano uses blake2b hash function for plutus data
async function blake2bHash(input:string): Promise<string> {
  const digest = await mod.crypto.subtle.digest(
    "BLAKE2B-256",
    decode(new TextEncoder().encode(input))
  );
  const string = mod.toHashString(digest)
  return string
}

async function readScript(name: string): Promise<L.MintingPolicy> {
  const validator = JSON.parse(await Deno.readTextFile("Plutus/assets/"+ name))
  return {
    type: "PlutusV2",
    script: validator.cborHex
  }
}

// ----- Haskell type
//
// data Test0 = BuiltinByteString
//
// a = "Hello World" :: Test0
//
// should be the following Test0.json file
//
// {
//   "bytes": "48656c6c6f20576f726c64"
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test0.json
//
// Typed version in lucid
console.log("Test0")
const Test0 = L.Data.Bytes()
type Test0 = L.Data.Static<typeof Test0>
const example0: Test0 = L.fromText("Hello World")
const example0PlutusData: string = L.Data.to<Test0>(example0,Test0)
console.log("typed cbor: "+ example0PlutusData)
// untyped version
const untypedCBOR0: string = L.Data.to(L.fromText("Hello World"))
console.log("untyped cbor: "+ untypedCBOR0)
console.log("client blake2b hash: 10506b1214d64bb7898d045946aeb4305ca3184ae10e95eaec9517db250a4184")
// the client command gives: 
const blake2bDigest0 = await blake2bHash(example0PlutusData)
console.log("lucid blake2b hash: "+blake2bDigest0)

// ----- Haskell type
//
// data Test1 = ConstructorTest1 BuiltinByteString
//
// a = ConstructorTest1 "Hello World" :: Test1
//
// should be the following Test1.json file
//
// {
//    "constructor": 0,
//    "fields": [
//        {
//            "bytes": "48656c6c6f20576f726c64"
//        }
//    ]
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test1.json
//
// Typed version in lucid
console.log("Test1")
const Test1 = L.Data.Object({
  constructor: L.Data.Bytes()
})
type Test1 = L.Data.Static<typeof Test1>
const example1: Test1 = {constructor: L.fromText("Hello World")}
const example1PlutusData: string = L.Data.to<Test1>(example1,Test1)
console.log("typed cbor: "+ example1PlutusData)
// untyped version
const untypedCBOR1: string = L.Data.to(new L.Constr(0,[L.fromText("Hello World")]))
console.log("untyped cbor: "+ untypedCBOR1)
console.log("client blake2b hash: b1643cb28fd06b389dd3909a8a115d97368f35c6cd8fb449d90c36675148e28c")
// the client command gives: 
const blake2bDigest1 = await blake2bHash(example1PlutusData)
console.log("lucid blake2b hash: "+blake2bDigest1)

// ----- Haskell type
//
// data Test2 = ConstructorTest2 Integer Integer
//
// a = ConstructorTest2 10 11 :: Test2
//
// should be the following Test2.json file
//
// {
//     "constructor": 0,
//     "fields": [
//         {
//             "int": 10
//         },
//         {
//             "int": 11
//         }
//     ]
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test2.json
//
// Typed version in lucid
console.log("Test2");
const Test2 = L.Data.Object(
  {
    object1: L.Data.Integer(),
    object2: L.Data.Integer() 
  }
)
type Test2 = L.Data.Static<typeof Test2>
const example2: Test2 = {object1: 10n, object2: 11n}
const example2PlutusData: string = L.Data.to<Test2>(example2,Test2);
console.log("typed cbor: "+ example2PlutusData);
// untyped version
const untypedCBOR2: string = L.Data.to(new L.Constr(0,[10n,11n]));
console.log("untyped cbor: "+ untypedCBOR2);
console.log("client blake2b hash: 605764b376177db95c8a6c192478e7a8e6c500c4efdabdf0d1c1c013c54923b0")
const blake2bDigest2 = await blake2bHash(example2PlutusData);
console.log("lucid blake2b hash: "+blake2bDigest2);

// ----- Haskell type
//
// data Test3 = Constructor1Test3 Integer | Constructor2Test3 Integer
//
// a = Constructor2Test3 11 :: Test3
//
// should be the following Test3.json file
//
// {
//  "constructor": 1,
//   "fields": [
//       {
//           "int": 11
//       }
//   ]
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test3.json
//
// Typed version in lucid
console.log("Test3");
const Test3 = L.Data.Enum([
  L.Data.Object({
    Constructor1Test3: L.Data.Tuple([L.Data.Integer()])
  }),
  L.Data.Object({
    Constructor2Test3: L.Data.Tuple([L.Data.Integer()])
  })
])
type Test3 = L.Data.Static<typeof Test3>
const example3: Test3 = {Constructor2Test3: [11n]}
const example3PlutusData: string = L.Data.to<Test3>(example3,Test3);
console.log("typed cbor: "+ example3PlutusData);
// untyped version
const untypedCBOR3: string = L.Data.to(new L.Constr(1,[11n]));
console.log("untyped cbor: "+ untypedCBOR3);
console.log("client blake2b hash: ad4b5cae11b679aed5df53eb25b22d63351c107dcf81ee865db23fa27ae1d74a")
const blake2bDigest3 = await blake2bHash(example3PlutusData);
console.log("lucid blake2b hash: "+blake2bDigest3);

// ----- Haskell type
//
// type Test4 = (BuiltinData, Test1)
// 
// a = Constructor "Hello World" :: Test1
// b = ("Hello World", a)  :: Test4
//
// should be the following Test4.json file
//
// {
//   "constructor": 0,
//   "fields": [
//       {
//           "bytes": "48656c6c6f20576f726c64"
//       },
//       {
//           "constructor": 0,
//           "fields": [
//               {
//                   "bytes": "48656c6c6f20576f726c64"
//               }
//           ]
//       }
//   ]
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test4.json
//
// Typed version in lucid
console.log("Test4");
const Test4 = L.Data.Object({
  fistElem: L.Data.Bytes(),
  secondElem: Test1
})
type Test4 = L.Data.Static<typeof Test4>
const example4: Test4 = {fistElem: L.fromText("Hello World"), secondElem: example1}
const example4PlutusData: string = L.Data.to<Test4>(example4,Test4);
console.log("typed cbor: "+ example4PlutusData);
// untyped version
const untypedCBOR4: string = L.Data.to(new L.Constr(0,[L.fromText("Hello World"), new L.Constr(0,[L.fromText('Hello World')])]));
console.log("untyped cbor: "+ untypedCBOR4);
console.log("client blake2b hash: 6942ba2fc66844f6ec0c7308fa3bcb89a06910077287d9f948e92a1cf93a9665")
const blake2bDigest4 = await blake2bHash(example4PlutusData);
console.log("lucid blake2b hash: "+blake2bDigest4);

// ----- Haskell type
//
// alwaysFailAddress :: Address
// alwaysFailAddress = Address {addressCredential = alwaysFailCredential, addressStakingCredential = Nothing}
//
// should be the following alwaysFailAddress.json file
//
// > printDataToJSON alwaysFailAddress 
// {
//     "constructor": 0,
//     "fields": [
//         {
//             "constructor": 1,
//             "fields": [
//                 {
//                     "bytes": "f130435a61f56c2f2f6304239f6d556df7291dd8628ce37a922fbc15"
//                 }
//             ]
//         },
//         {
//             "constructor": 1,
//             "fields": []
//         }
//     ]
// }
//
// This json file can be converted to plutus data and hashed via the command
// 
// cardano-cli transaction hash-script-data --script-data-file Test4.json
//
// Typed version in lucid
console.log("Test always fail address");
const PubKeyHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
type PubKeyHash = L.Data.Static<typeof PubKeyHash>;

const ValidatorHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
type ValidatorHash = L.Data.Static<typeof ValidatorHash>;

const Credential = L.Data.Enum([
    L.Data.Object({ PubKeyCredential: L.Data.Tuple([PubKeyHash]) }),
    L.Data.Object({ ScriptCredential: L.Data.Tuple([ValidatorHash]) })
]);
type Credential = L.Data.Static<typeof Credential>;

//In an address, a chain pointer refers to a point of the chain containing a stake key registration certificate. A point is identified by 3 coordinates:
// An absolute slot number
// A transaction index (within that slot)
// A (delegation) certificate index (within that transaction)
const StakingPtr = L.Data.Object({
    slotNumber: L.Data.Integer(),
    transactionIndex: L.Data.Integer(),
    certificateIndex: L.Data.Integer()
});
type StakingPtr = L.Data.Static<typeof StakingPtr>;

const StakingCredential = L.Data.Enum([
    L.Data.Object({ StakingHash: L.Data.Tuple([Credential]) }),
    L.Data.Object({ StakingPtr: L.Data.Tuple([StakingPtr]) })
]);
type StakingCredential = L.Data.Static<typeof StakingCredential>;

const Address = L.Data.Object({
    addressCredential: Credential,
    addressStakingCredential: L.Data.Nullable(StakingCredential)
});
type Address = L.Data.Static<typeof Address>;

// import always fail script
const validatorAlwaysFail: L.SpendingValidator = await readScript("AlwaysFail.plutus");
const addressAlwaysFail: L.Address = lucid.utils.validatorToAddress(validatorAlwaysFail);
const details: L.AddressDetails = L.getAddressDetails(addressAlwaysFail);
const addrAlwaysFail: Address = {addressCredential: { ScriptCredential: [details.paymentCredential.hash] }, addressStakingCredential: null};
const addressPlutusData: string = L.Data.to<Address>(addrAlwaysFail,Address);
console.log("typed cbor: "+ addressPlutusData);
console.log("client blake2b hash: c05e0b92f78a14e5922d68befbea9af882f7db758b0cf949d0543de69da7eea4")
const blake2bDigestAddr = await blake2bHash(addressPlutusData);
console.log("lucid blake2b hash: "+blake2bDigestAddr);
