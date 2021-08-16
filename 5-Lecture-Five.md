# Lecture Five

>"It might be true that there are six billion people in the world and counting. Nevertheless, what you do makes a difference. It makes a difference, first of all, in material terms. It makes a difference to other people and it sets an example." <br />
> ― Robert Solomon, Waking Life

*Recently, I noticed an up-tick in questions about Git and GitHub on the IOHK Plutus Pioneer Discord. For this reason, I took a few hours out to write a brief introduction to Git (and how ones local git repo ties to, in this instance, the remote IOHK repo). Really, you can find all the answers you should need at [https://git-scm.com/](https://git-scm.com/), thus, I have included some links to specific pages on git-scm. You can view the introduction I wrote [here](0-Git-Introduction.md). If any further questions pop up, I'll do my best to answer and then append answers to the introduction document. Again, [You Can Read The Introduction to Git I Wrote Here.](0-Git-Introduction.md)*

### 1. Introduction

*Currently Being Written*

**Let it be known that I have had to take some time out to put together a catalyst proposal. Watch this space.**

### 2. Genesis: And God Said Let Us Make An AssetClass

*Turns out God seemed to like the image of Ada Lovelace [[1]](#1) more than the image of Man!*

All jokes aside *(I told you this going to be a somewhat creative technical document, you were warned)*, I suppose the first question is: What Is A Native Token?

*At this point, I think most 'techy' people do understand the concept of Cardano's Native Tokens, NFTs (I would love to see fractional NFTs by the way [[2]](#2), and perhaps some nicer ways of implementing NFT payloads without bloating the chain) and perhaps even the notion of wrapped tokens. I do have to continually remind myself that this is an extremely new multi-disciplinary subject that... Without trying to sound arrogant (because I don't understand some of this stuff myself) most people just do not understand this technology.*

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

As you are now aware, scripts for creating and destroying native tokens sit at an address which results from hashing the CurrencySymbol. However, if you recall the UTxO model, the input must equal the output (minus the fees). This could never be true of the EUTxO model, otherwise it would be impossible to create Native Tokens. Why? Because the fees associated with running Native Token scripts depend on the size of the transaction (in bytes, not in value) and also the size of the scripts that need to be run to validate the transaction. In computer science terms, the fees depend on the algorithmic complexity of the arbitrary logic which sites at the script address. It sounds like it is simply the space complexity which determines the fees, but I would imagine it is both space and time. In short: <code>O(x) ~ fees | x : script</code>.

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

<hr />

-

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

1. ajsjhasdasd

### Appendix

Value.hs

Ada.hs