# Lecture Five

>"It might be true that there are six billion people in the world and counting. Nevertheless, what you do makes a difference. It makes a difference, first of all, in material terms. It makes a difference to other people and it sets an example." <br />
> ― Robert Solomon, Waking Life

*Recently, I noticed an up-tick in questions about Git and GitHub on the IOHK Plutus Pioneer Discord. For this reason, I took a few hours out to write a brief introduction to Git (and how ones local git repo ties to, in this instance, the remote IOHK repo). Really, you can find all the answers you should need at [https://git-scm.com/](https://git-scm.com/), thus, I have included some links to specific pages on git-scm. You can view the introduction I wrote [here](0-Git-Introduction.md). If any further questions pop up, I'll do my best to answer and then append answers to the introduction document. Again, [You Can Read The Introduction to Git I Wrote Here.](0-Git-Introduction.md)*

### 1. Introduction

Within this particular set of lecture notes we will be addressing Native Tokens on Cardano. Firstly, a discussion surrounding the building blocks of any such Native Token is provided. Secondly, the definition of a Native Token is presented, along with how any such token may be contrasted with Ada / Lovelace. Furthermore, the concepts and implementation details surrounding Policies (to create or destroy Native Tokens under well defined constraints) are demonstrated. Finally, an investigation into Non Fungible Tokens (NFTs) and how they relate to Native Tokens is surveyed.

**Let it be known that I have had to take some time out to put together a catalyst proposal. Watch this space.**

### 2. Genesis: And God Said Let Us Make An AssetClass

*Turns out God seemed to like the image of Ada Lovelace [[1]](#1) more than the image of Man!*

All jokes aside *(I told you this going to be a somewhat creative technical document, you were warned)*, I suppose the first question is: What Is A Native Token?

*At this point, I think most 'techy' people do understand the concept of Cardano's Native Tokens, NFTs (I would love to see fractional NFTs by the way [[2]](#2), and perhaps some nicer ways of implementing NFT payloads without bloating the chain).*

### 2.1 What Is A Native Token?

To answer that question, we are **required** to understand [AssetClasses](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#g:3) in order to understand Native Tokens. An AssetClass is a type, which means it takes a constructor, so let's deconstruct that constructor!

<code>unAssetClass :: (CurrencySymbol, TokenName)</code>

So, we have a type 'newtype' AssetClass which takes a 'newtype' [CurrencySymbol](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:CurrencySymbol) and a 'newtype' [TokenName](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:TokenName). Don't worry, CurrencySymbol and TokenName are both just wrappers for a [ByteString](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:ByteString). Further, if one sets <code>-XOverloadedStrings</code> then you can simply use a string literal for any ByteString (the CurrencySymbol does have to be written in hexadecimal though). This does in fact mean that a Native Token is defined by the combination of a CurrencySymbol and a TokenName. **There is one exception, and we'll get to that now.**

### 2.2 Ada Lovelace

Apart from being one of the first ever programmers on the planet and correct about Babbage and his vision for computing... Lovelace is in fact an AssetClass (when it comes to Cardano anyway). Meaning, it is a Native Token (of sorts). However, Native Tokens can be minted (otherwise how would one bring one into existence), and burnt (destroyed). But, you cannot mint anymore Lovelace / Ada, what gives?

As in §2.1, a Native Token that can be **minted** and / or **burned** within a set of constraints requiring a CurrencySymbol and a TokenName. Shortly, it will be demonstrated that Lovelace does not have a CurrencySymbol and therefore cannot be minted or burned (which is a good thing). It means that there is only a set amount of lovelace / ada within the ecosystem.

<pre><code>import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Ada
:set -XOverLoadedStrings
:t adaSymbol
adaSymbol :: CurrencySymbol
adaSymbol

adaToken :: TokenName
adaToken
""
-- how do we create lovelace for testing purposes?
:t lovelaceValueOf
lovelaceValueOf :: Integer -> Value
lovelaceValueOf 123
Value (Map [(...,Map [("", 123)])]) -- a currency symbol exists at ...
-- This is actually extremely similar to how text search indexing works: word -> (page, freq)
</code></pre>

### 2.3 Native Tokens: The Importance Of CurrencySymbols

It has been made clear that CurrencySymbols must be written in hexadecimal. The reason for this is because the script address generated to contain the logic (to define the constraints) for minting and burning Native Tokens uses a hash function of the CurrencySymbol. If you think about it, it makes perfect sense: if ada lovelace has no CurrencySymbol or TokenName, then there cannot be a script address responsible for minting and / or burning ada lovelace. However, as the AssetClass constructor takes two ByteStrings as a constructor, the first of which is the CurrencySymbol, hashing the hex value of contained within would produce a unique address where you could drop a minting policy script.

### 2.4 The Basics: Minting Policy Scripts

As you are now aware, scripts for creating and destroying native tokens sit at an address which results from hashing the CurrencySymbol. However, if you recall the UTxO model, the input must equal the output (minus the fees). This could never be true of the EUTxO model, otherwise it would be impossible to create Native Tokens. Why? Because we can create transactions which are not simply 'spending'. Further, the fees associated with running Native Token scripts depend on the size of the transaction (in bytes, not in value) and also the size of the scripts that need to be run to validate the transaction. In computer science terms, the fees depend on the algorithmic complexity of the arbitrary logic which sites at the script address. It sounds like it is simply the space complexity which determines the fees, but I would imagine it is both space and time. In short: <code>O(x) ~ fees | x : script</code>.

### 3. Creating A Native Token

Enough chit chat, let's go ahead and create a native token, or two.

Let us set overloaded strings such that we can use string literals for the ByteString constructor, then we'll import everything we need to apply what we have learnt thus far (Value.hs and Ada.hs).

<pre><code>Prelude Week05.Free> :set -XOverloadedStrings 
Prelude Week05.Free> import Plutus.V1.Ledger.Value
Prelude Plutus.V1.Ledger.Value Week05.Free> import Plutus.V1.Ledger.Ada
</code></pre>

Now, we're about to create some Native Tokens. But let's stop for a second because there is something important we need to review. It just so happens that Values are instances of Monoids which means they can be combined in various ways. Thus, it is likely a good idea to review some information about Monoids.

**The Monoid Laws**

> Before moving on to specific instances of Monoid, let's take a brief look at the monoid laws. We mentioned that there has to be a value that acts as the identity with respect to the binary function and that the binary function has to be associative. It's possible to make instances of Monoid that don't follow these rules, but such instances are of no use to anyone because when using the Monoid type class, we rely on its instances acting like monoids. Otherwise, what's the point? That's why when making instances, we have to make sure they follow these laws:<br /><br />
> mempty `mappend` x = x <br />
> x `mappend` mempty = x<br />
> (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)<br /><br />
> The first two state that mempty has to act as the identity with respect to mappend and the third says that mappend has to be associative i.e. that it the order in which we use mappend to reduce several monoid values into one doesn't matter. **Haskell doesn't enforce these laws, so we as the programmer have to be careful that our instances do indeed obey them.** [[3]](#3)

Now that we're ready to move on, let's get cracking and write some code.

<pre><code>Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t singleton
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 72
Value (Map [(a8ff,Map [("ABC",72)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 72 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",72),("XYZ",100)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> let v = singleton "a8ff" "ABC" 72 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> v
Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",72),("XYZ",100)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t valueOf
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "XYZ"
100
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "ABC"
72
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" ""
0
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "abcsds"
0
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t flattenValue
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> flattenValue v
[(a8ff,"ABC",72),(a8ff,"XYZ",100),(,"",42)]
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free>
</code></pre>

### 4. Minting Policies

* Refresher On validation
* No public key address, script address
* UTxO sits at such a script address
* Tx tries to consume UTxO as an input
* For each script input, the co-responding validation script is run
* Validation script (as input) gets:
	* Datum (comes from the UTxO)
	* Redeemer, which comes from the Tx
	* Context (pretty sure context is part of the UTxO)
		* two fields: ScriptPurpose, txInfo
	* Every ScriptPurpose until now always had the Spending txOutRef
	* txOutRef is a reference to the UTxO we're trying to consume
	* txInfo has all the context information about the Tx which is trying to be validated
* For monetary policies, for minting policies: if the txForgeField (txInfoForge is != 0)
* Then txInfoForge can contain a bag of asset classes (map => map) [Value]
* IFF txInfoForge is != 0, then for every CurrencySymbol in this bag that is being forged:
* The co-responding script sitting @ hash(Currencysymbol) is going to be ran
* These minting policy scripts only have 2 validator inputs: redeemer and the context
* Minting Policy Script only have 2 inputs: Redeemer and the Context, No datum
* Tx provides redeemer (bool) + redeemer for all script inputs
* To summerise:
* IFF the Transaction Forge Field (Context -> txInfo -> txInfoForge) is non-zero...
* Meaning that a Value type sits within that transaction attribute...
* And remember, Value is a map->map, like an indexing data structure... THEN:
* For each currency symbol:
* A hash of the symbol is taken which produces a script address, AND
* The script at that address is executed, AND
* For each script where the redeemer returns True,
* A Transaction is created which will create or burn the Native Token associated with that script...
* It will do so in accordance with the scripts arbitrary logic...
* The fee to be paid depends on the computational complexity of the script located at the address derived from hashing the currency symbol, so long as the redeemer returned True.

<pre><code>{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
</code></pre>

Similarly to validators for spending, when we are producing a 'validator' that implements some kind of monetary policy, such as minting Native Tokens (known as a minting policy), we provide similar arguments as we would to a spending validator. However, it should be noted that there is simply no purpose for a datum parameter for any given policy, it is non-sensical. Thus, there exists only two parameters for any given policy. The two arguments we typically provide are (although, you can have parameterised policies, just as you may parameterised validators), as you might expect: the redeemer and the context.

At the very least we require a redeemer because we need to know whether minting and / or burning of a given token is permitted. The redeemer is required to satisfy the arbitrary logic found at the script address. Furthermore, the context is also required because we must have access to every CurrencySymbol (which is derived through: context.txInfo.txInfoForge :: Value) such that we can produce the script addresses (by applying a hashing algorithm to the CurrencySymbol(s) ByteString). 

As the Value is a mapping from X to a mapping of Y to Z, where X and Y are ByteStrings and Z is an Integer, it is possible to have multiple CurrencySymbols that point to multiple Tokens with varying degrees of amount. See the image below.

![./img/map-to-map.jpg](./img/map-to-map.jpg)

*Note: Ada Lovelace is not part of the data structure. She existed a long time ago...*

The above example is **the simplest** example you might expect to see, because it's a minting policy which always returns True (you can mint or burn as many tokens as you like). This is demonstrated through the nature of the <code>mkPolicy</code> function within free.hs (I have also included the section which compiles the policy to Plutus-core and the procedure used to generate a script address):

<pre><code>{-# INLINABLE mkPolicy #-} -- required for the oxford brackets
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
</pre></code>

In the above instance, we are using (for the most part) typed Haskell to compile to Plutus-core (because, well, you should). The problem that we would normally encounter when utilising the oxford brackets is solved using a library function, as indicated in the above snippet by <code>Scripts.wrapMintingPolicy</code>.

Notice how the declaration of <code>mkPolicy</code> takes (as its arguments): unit and ScriptContext with a return type of Bool. The reason why we're able to use unit as an argument for the redeemer is because it does implements the isData instance, otherwise we would also have to create a typed redeemer. So, within this example, regardless of the script context and the redeemer, the policy is **always going to return True.**

Finally, in terms on on-chain code, the script address is produced by declaring a CurrencySymbol and assigning the result of <code>scriptCurrencySymbol policy</code> to it. This takes the compiled policy script and hashes it to leave us with the address at which the script will sit.

**Off-Chain Code:**

	data MintParams = MintParams
	    { mpTokenName :: !TokenName
	    , mpAmount    :: !Integer
	    } deriving (Generic, ToJSON, FromJSON, ToSchema)
	
	type FreeSchema = Endpoint "mint" MintParams
	
	mint :: MintParams -> Contract w FreeSchema Text ()
	mint mp = do
	    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
	        lookups = Constraints.mintingPolicy policy
	        tx      = Constraints.mustMintValue val
	    ledgerTx <- submitTxConstraintsWith @Void lookups tx
	    void $ awaitTxConfirmed $ txId ledgerTx
	    Contract.logInfo @String $ printf "forged %s" (show val)
	
	endpoints :: Contract () FreeSchema Text ()
	endpoints = mint' >> endpoints
	  where
	    mint' = endpoint @"mint" >>= mint
	
	mkSchemaDefinitions ''FreeSchema
	
	mkKnownCurrencies []

**Run Outside The Playground:**

	test :: IO ()
	test = runEmulatorTraceIO $ do
	    let tn = "ABC"
	    h1 <- activateContractWallet (Wallet 1) endpoints
	    h2 <- activateContractWallet (Wallet 2) endpoints
	    callEndpoint @"mint" h1 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = 555
	        }
	    callEndpoint @"mint" h2 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = 444
	        }
	    void $ Emulator.waitNSlots 1
	    callEndpoint @"mint" h1 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = -222
	        }
	    void $ Emulator.waitNSlots 1

### 4.1 Realistic Policies

**Policies Should Maintain Well Defined Constraints**

	{-# LANGUAGE DataKinds           #-}
	{-# LANGUAGE DeriveAnyClass      #-}
	{-# LANGUAGE DeriveGeneric       #-}
	{-# LANGUAGE FlexibleContexts    #-}
	{-# LANGUAGE NoImplicitPrelude   #-}
	{-# LANGUAGE OverloadedStrings   #-}
	{-# LANGUAGE ScopedTypeVariables #-}
	{-# LANGUAGE TemplateHaskell     #-}
	{-# LANGUAGE TypeApplications    #-}
	{-# LANGUAGE TypeFamilies        #-}
	{-# LANGUAGE TypeOperators       #-}
	
	module Week05.Signed where
	
	import           Control.Monad          hiding (fmap)
	import           Data.Aeson             (ToJSON, FromJSON)
	import           Data.Text              (Text)
	import           Data.Void              (Void)
	import           GHC.Generics           (Generic)
	import           Plutus.Contract        as Contract
	import           Plutus.Trace.Emulator  as Emulator
	import qualified PlutusTx
	import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
	import           Ledger                 hiding (mint, singleton)
	import           Ledger.Constraints     as Constraints
	import qualified Ledger.Typed.Scripts   as Scripts
	import           Ledger.Value           as Value
	import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
	import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
	import           Playground.Types       (KnownCurrency (..))
	import           Prelude                (IO, Show (..), String)
	import           Text.Printf            (printf)
	import           Wallet.Emulator.Wallet
	
	{-# INLINABLE mkPolicy #-}
	mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
	mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh
	
	policy :: PubKeyHash -> Scripts.MintingPolicy
	policy pkh = mkMintingPolicyScript $
	    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
	    `PlutusTx.applyCode`
	    (PlutusTx.liftCode pkh)
	
	curSymbol :: PubKeyHash -> CurrencySymbol
	curSymbol = scriptCurrencySymbol . policy
	
	data MintParams = MintParams
	    { mpTokenName :: !TokenName
	    , mpAmount    :: !Integer
	    } deriving (Generic, ToJSON, FromJSON, ToSchema)
	
	type SignedSchema = Endpoint "mint" MintParams
	
	mint :: MintParams -> Contract w SignedSchema Text ()
	mint mp = do
	    pkh <- pubKeyHash <$> Contract.ownPubKey
	    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
	        lookups = Constraints.mintingPolicy $ policy pkh
	        tx      = Constraints.mustMintValue val
	    ledgerTx <- submitTxConstraintsWith @Void lookups tx
	    void $ awaitTxConfirmed $ txId ledgerTx
	    Contract.logInfo @String $ printf "forged %s" (show val)
	
	endpoints :: Contract () SignedSchema Text ()
	endpoints = mint' >> endpoints
	  where
	    mint' = endpoint @"mint" >>= mint
	
	mkSchemaDefinitions ''SignedSchema
	
	mkKnownCurrencies []
	
	test :: IO ()
	test = runEmulatorTraceIO $ do
	    let tn = "ABC"
	    h1 <- activateContractWallet (Wallet 1) endpoints
	    h2 <- activateContractWallet (Wallet 2) endpoints
	    callEndpoint @"mint" h1 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = 555
	        }
	    callEndpoint @"mint" h2 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = 444
	        }
	    void $ Emulator.waitNSlots 1
	    callEndpoint @"mint" h1 $ MintParams
	        { mpTokenName = tn
	        , mpAmount    = -222
	        }
	    void $ Emulator.waitNSlots 1


### 5. Non Fungible Tokens

	{-# LANGUAGE DataKinds           #-}
	{-# LANGUAGE DeriveAnyClass      #-}
	{-# LANGUAGE DeriveGeneric       #-}
	{-# LANGUAGE FlexibleContexts    #-}
	{-# LANGUAGE NoImplicitPrelude   #-}
	{-# LANGUAGE OverloadedStrings   #-}
	{-# LANGUAGE ScopedTypeVariables #-}
	{-# LANGUAGE TemplateHaskell     #-}
	{-# LANGUAGE TypeApplications    #-}
	{-# LANGUAGE TypeFamilies        #-}
	{-# LANGUAGE TypeOperators       #-}
	
	module Week05.NFT where
	
	import           Control.Monad          hiding (fmap)
	import qualified Data.Map               as Map
	import           Data.Text              (Text)
	import           Data.Void              (Void)
	import           Plutus.Contract        as Contract
	import           Plutus.Trace.Emulator  as Emulator
	import qualified PlutusTx
	import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
	import           Ledger                 hiding (mint, singleton)
	import           Ledger.Constraints     as Constraints
	import qualified Ledger.Typed.Scripts   as Scripts
	import           Ledger.Value           as Value
	import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
	import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
	import           Playground.Types       (KnownCurrency (..))
	import           Prelude                (IO, Semigroup (..), Show (..), String)
	import           Text.Printf            (printf)
	import           Wallet.Emulator.Wallet
	
	{-# INLINABLE mkPolicy #-}
	mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
	mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
	                          traceIfFalse "wrong amount minted" checkMintedAmount
	  where
	    info :: TxInfo
	    info = scriptContextTxInfo ctx
	
	    hasUTxO :: Bool
	    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
	
	    checkMintedAmount :: Bool
	    checkMintedAmount = case flattenValue (txInfoForge info) of
	        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
	        _                -> False
	
	policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
	policy oref tn = mkMintingPolicyScript $
	    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
	    `PlutusTx.applyCode`
	    PlutusTx.liftCode oref
	    `PlutusTx.applyCode`
	    PlutusTx.liftCode tn
	
	curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
	curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
	
	type NFTSchema = Endpoint "mint" TokenName
	
	mint :: TokenName -> Contract w NFTSchema Text ()
	mint tn = do
	    pk    <- Contract.ownPubKey
	    utxos <- utxoAt (pubKeyAddress pk)
	    case Map.keys utxos of
	        []       -> Contract.logError @String "no utxo found"
	        oref : _ -> do
	            let val     = Value.singleton (curSymbol oref tn) tn 1
	                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
	                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
	            ledgerTx <- submitTxConstraintsWith @Void lookups tx
	            void $ awaitTxConfirmed $ txId ledgerTx
	            Contract.logInfo @String $ printf "forged %s" (show val)
	
	endpoints :: Contract () NFTSchema Text ()
	endpoints = mint' >> endpoints
	  where
	    mint' = endpoint @"mint" >>= mint
	
	mkSchemaDefinitions ''NFTSchema
	
	mkKnownCurrencies []
	
	test :: IO ()
	test = runEmulatorTraceIO $ do
	    let tn = "ABC"
	    h1 <- activateContractWallet (Wallet 1) endpoints
	    h2 <- activateContractWallet (Wallet 2) endpoints
	    callEndpoint @"mint" h1 tn
	    callEndpoint @"mint" h2 tn
	    void $ Emulator.waitNSlots 1


### References

<a href="#1" id="1">[1]</a> Isaac, A.M., 2018. <br />
Computational thought from Descartes to Lovelace. <br />
The Routledge Handbook of the Computational Mind, pp.9-22. <br />

<a href="#2" id="2">[2]</a> Algorand, MAY 07, 2021. <br />
How Algorand Democratizes the Access to the NFT Market with Fractional NFTs. <br />
<https://www.algorand.com/resources/blog/algorand-nft-market-fractional-nfts> <br />

<a href="#3" id="3">[3]</a> Lipovaca, M., 2011. <br />
Learn you a haskell for great good!: a beginner's guide. <br />
no starch press. <br />

### Footnotes

1. TODO: Add Footnote

### Appendix

Value.hs

TODO: Add Value.hs

Ada.hs

TODO: Add Ada.hs

**Random Notes Whilst Listening To Lecture...**

* UTxO : Address & Value
* EUTxO : Address & Value & Datum
* newType New Type : Value
	* Value
		* getValue :: Map CurrencySymbol (Map TokenName Integer)
		* If I understand correctly, this is embedding Native Tokens or Metadata Or Sorts Into Another Native Token?
		* Value === Map AssetClass Integer
		* This makes sense, because the value is an AssetClass which contains an Integer (Value) - although I would have thought this would need to be a floating point number.
		* Value returns the number of units which are in each AssetClass
* The new types: TokenName and CurrencySymbol implement the IsString class, so we can use -XOverLoadedStrings to enter string literals.
* Since TokenName and CurrencySymbol are both just essentially wrappers for a ByteString, we can enter string literals to their constructors.
* Values Containing Native Tokens
* :t singleton -- currencysymbol token name integear <- args
* Singleton :: CurrencySymbol -> TokenName -> Integer -> Value
* Singleton takes, as arguments, a CurrencySymbol (which has to be a hex value), a TokenName (which can be a string literal) and an Integer (which is the number of tokens) and it will return a Value, which if you remember correctly, is a mapping, similar to what you might see when you're running an indexing algorithm (CurrencySymbol -> (TokenName, NumberOfTokens)).
* Why do we need a currency symbol + token name?
* Why don't we just use a single identifier?
* Why does the currency symbol need to be built from hexadecimal?
* This is where minting policies come in.
* In general: a Tx cannot create a delete tokens.
* Everything goes in, comes out
* exception: fees
* fees depend on the size of the transaction (in bytes, not in value) and also the size of the scripts that need to be run to validate the transaction (space complexity, I believe? General computational complexity, I would have thought, O(x) | x ~ fees)
* If this was true for all cases, then we could never create native tokens
* Hence: Minting Policies & relevance of currency sybol
* reason why currency symbol bytestring needs to be hexadec is because it's the hash of a script
* this scipt is called the mintng policy
* if we have a tx where we want to create or burn native tkens, then
* for each native token we want to create or burn
* the currency symbol is looked up as a hash of the script
* so the co-responding script must be contained within the transaction
* that script is executed + other validation scripts
* simiar to validation scrpts that validate inputs
* this script needs to know whether the Tx has the right to mint or burn tokens
* and since ada has no currency symbol, there is no hash, there is no script, there is no minting, there is no burning.
* all ada that exists comes from genesis block
* total amount of ada in the system is fixxed
* only custom native tokens can have custom minting policies and can be minted + burned under certain conditions
* Next we'll look at a minting policy script, it's similar to validation script, but not identical