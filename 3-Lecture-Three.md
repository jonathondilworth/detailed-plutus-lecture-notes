# 3. Lecture Three

### 1. Introduction

*Note: typically you write the introduction last...*

Within this set of lecture notes, the following will be discussed. Firstly, memory issues associated with the local 'Playground' server (where our Haskell smart contracts are being ran) are addressed. Secondly, a reintroduction to the extended unspent transaction output model (EUTxO) is provided. Furthermore, minting tokens, certifying and (stake-related) rewarding is discussed. This is followed by a description of the parameters that validator scripts are provided. Then on-chain and off-chain code is briefly discussed.

*Note: introduction is unfinished, once I get the chance to work through the whole lecture, the introduction will be completed and a summary will be provided at the end, as is typical within this type of documentation.*

### 2. Playground Memory Issues

You are now able to modify the timeout in the Plutus Playground Server (located within the Plutus Playground Client folder) to any number of minutes. This is accomplished by running the following command:

	plutus-playground-server -i 120s
	
Note that you can modify the value of 120s to any amount, this will set the timeout in XXX seconds <code>XXXs</code>.

-

### 3. Quick Refresh On The (E)UTxO Model

(E)UTxO stands for **Extended Unspent Transaction Output Model**.

*Let me just bring you back up to speed on UTxO...*

This model is similar to how BitCoin (BTC) manages unspent digital assets held by any given wallet (*these assets can be thought of as money...* in BTCs case, these assets would be BitCoin). However, with BTC, the model is simply built through very simple UTxOs (unextended).

Given a simple UTxO, there is a **redeemer** and a **validator**. In order to spend any unspent transaction output, the redeemer can be thought of as a cryptographic key which is passed to the validator. Once any given UTxO is attempted to be spent, the validator is then responsible for verifying the chain of ownership by using the redeemer to verify that the UTxO belongs to whichever wallet is attempting to create a new UTxO.

***With an (E)UTxO model, you have a script at a given address, a redeemer, context and datum...*** Validation must still occur in order to spend any given (E)UTxO. However, the script address is contains a reference to a set of instructions. The instructions **(the script)** contains arbitrary logic, some of which is responsible for creating a validator (plus a possible set of numerous constraints under which the validator may or may not verify the chain of ownership of any given (E)UTxO).

Furthermore, the (E)UTxO model has the ability to facilitate ownership of multiple types of asset (for example non fungible tokens can exist within a (E)UTxO model. Even the ownership of other coins can be transferred from one wallet to another given the EUTxO model).

### 3.1 EUTxO Scripts

EUTxO scripts are held at a script address. During lecture two we saw a low level implementation of a validator within a script where all three arguments were defined as the Haskell type: <code>Data</code>:

<pre><code>mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()	
validator :: Validator
validator = mkValidatorScript $\$(PlutusTx.compile [|| mkValidator ||])
</code></pre>

In practice this is **not used**. We instead use the typed version. This is where data and redeemer can be custom types, as long as they implement the <code>IsData</code> type class. The third argument (the Context) is must be of type <code>ScriptContext</code>.

In the examples we have seen so far, we've only been examining the data and the redeemer. We have never given much thought to the context, but the context is of course very important. It can support a given state, allowing us to create types of state machines within 

The <code>Context</code> is of type: <code>ScriptContext</code> and can be found within:

<pre><code>plutus-ledger-api</code></pre>

This is a package that, until now, we have not needed. But now, we do need it, so it has been included within the cabal.project file for the third week. The Context can be found within:

<pre></code>Plutus.V1.Ledger.Contexts</code></pre>

The <code>ScriptContext</code> is a record type with two fields: <code>TxInfo</code> and <code>ScriptPurpose</code>. The <code>ScriptPurpose</code> is defined within the same module and it (rather obviously) describes for which purpose the script is run. For example, to spend, to certify, to reward or to mint (an NFT for example), etc.

The most important purpose for us is the <code>Spending TxOutRef</code>, this is what we have mostly talked about within the Extended UTxO model.

This is when a script is run to validate spending input for a transaction.

### 3.2 Other Important Purposes

**Minting**: This is important for when you want to define a native token. For example, the ScriptPurpose may use the Minting constructor to create a native token which describes under which condition the token may be minted or burned.

**Rewarding**: Related to staking (in some manner, yet to be explained).

**Certifying:** Related to certificates, like delegation certification.

*For now, we are concentrating on the spending purpose.*

### 3.3 Context - TxInfo (Acronym for Transaction Info) — **Possibily Outdated**

The TxInfo Data Type describes the transaction<sup>[1](#ft1)</sup>, which is to say it has fields for:

* txInfoInputs :: **[TxInInfo]** — ALL the inputs of the transaction.
* txInfoOutputs :: **[TxOut]** — ALL the outputs of the transaction.
* txInfoFee :: **Value** — The fee paid to consume the transaction.
* TxInfoForge :: **value** — Either the number of newly created (forged) Native Tokens [[2]](#2) or if negative, the amount of newly burned Native Tokens.
* txInfoDCert :: ***[TxInfoDCert]*** — List of certificates, such as delegation certificates. ***(Possibly Deprecated)***
* txInfoWdrl :: **[(StakingCredential, Integer)]** — Staking Reward Withdrawal ***(Possibly Deprecated)***
* txInfoValidRange :: **POSIXTimeRange** — Time range in which the transaction is valid. ***(Possibly Modified)***
* txInfoSignatories :: [PubKeyHash] — List of public keys that have signed the transaction.
* txInfoData :: [(DatumHash, Datum)] — The output value of the Tx which have been spent (required), Lars uses the phrase:


<pre><code>Spending Transactions Have To Include The Datum Of The Transactions They've Spent (The Script Output).

Producing transactions that have spent any given output only have to include the hash (Which hash exactly?) - dictionary of 'datum-hash' to hash to full datum values to a given hash?

However, Producing Transactions Can Optionally Do That.</code></pre>

* txInfoId :: TxId — Hash of the pending transaction (excluding witnesses)

### 3.4 Context - TxInfo (Current)

* txInfoInputs :: [TxInInfo] — Transaction inputs
* txInfoOutputs :: [TxOutInfo] — Transaction outputs
* txInfoFee :: Value — The fee paid by this transaction.
* txInfoForge :: Value — The Value forged by this transaction.
* txInfoValidRange :: SlotRange — The valid range for the transaction.
* txInfoForgeScripts :: [MonetaryPolicyHash]
* txInfoSignatories :: [PubKeyHash] — Signatures provided with the transaction
* txInfoData :: [(DatumHash, Datum)]	 
* txInfoId :: TxId — Hash of the pending transaction (excluding witnesses)

### 3.5 On-Chain VS Off-Chain Validation

One of the nice things about Cardano is that validation can be tested and verified as spendable off-chain. However, due to time — well latency [[2]](#2)[[3]](#3), there is always a chance that a given UTxO that you — well, the wallet trying to consume said UTxO — has already been spent by another transaction on-chain, causing a reversal. You don't, however loose any funds.

Furthermore, there is always a chance that the time that the off-chain code is executed and the contract is found to be valid, if the txValidRange *(time)* fell inside the time of validation off-chain but outside the time of validation on-chain, the validation will fail **on-chain** and the contract will not be executed. However, if the time does fall into the txValidRange, then the contract is completely deterministic. The only non-deterministic property of these contracts is the time at which it may or may not be executed, as time is continuous and impossible to test for. When writing scripts and testing, it is likely the case that you will have to assume that the function for checking if the time is valid or not is TRUE. You may also write cases where it is FALSE to be exhaustive, but you know (in this case) the script will simply fail and the UTxO will not be consumed.

**By default, if this parameter is not set manually, the time slot / POSIXTIME is set to 'infinite', and as such - this initial check will always pass.**

*In short: "The trick is" to do a time check before attempting to validate the transaction (initially off-chain), then when you attempt to spend some UTxO on-chain, ideally it'll validate, but there is always the possibility (due to time latency) that validation is not possible. So, there is time on one hand and determinism on the other, essentially.*

**IMPORTANT: The consensus algorithm uses SLOT time, not POSIXTIME; which is probably why the current TxInfo uses SlotRange, rather than POSIXTimeRange.**

As it stands in lecture three, we're still using POSIXTime, apparently it is easy to move between POSIXTime and Slot 'Time'. This is simply a slight complication between Plutus and Ouroboros. We know that if a parameter change occurs, we will always know 36 hours in the future (we'll know if there is a hard fork or changes to be made within 36 hours).

*Note: it would appear now that POSIXTime has changed to SlotRange for the reasoning Lars outlines about time? General observation, may be wrong.*

### 3.5.1 General Checks (Summary)

* All the inputs are present.
* The balances add up.
* The fees are included.
* The 'time-range' is checked (node checks the current time and compares it to the time-range specified by the transaction. If the current time does not fall into this time range, then then validation fails immediately, without ever running the validator scripts.

However, if the time-range does fall into this interval, then validation is completely deterministic again. This is just a static piece of data attached to the transaction. Thus, the result of validation **does not** depend on when it is run.

* By default, all transactions use an infinite time interval.

### 3.6 Time Intervals

Specifying a time interval in Haskell can be done in the following manner (using one of many types of constructors, which can be found within the documentation) [[4]](#4) as is outlined using the repl:

<pre><code>> cabal repl
...
> import Plutus.V1.Ledger.Interval
-- Interval from a to b
> interval (10 :: Integer) 20
Interval {ivFrom = LowerBound (Finite 10) True, ivTo = UpperBound (Finite 20) True}
> member 9 $ interval (10 :: Integer) 20
False
> member 10 $ interval (10 :: Integer) 20
True
> member 11 $ interval (10 :: Integer) 20
True
> member 20 $ interval (10 :: Integer) 20
True
> member 21 $ interval (10 :: Integer) 20
False
-- Interval from 30 to +
> 21 $ from (30 :: Integer)
False
> 30 $ from (30 :: Integer)
True
> 30000 $ from (30 :: Integer)
True
-- Interval to 30 from -
> 30000 $ to (30 :: Integer)
False
> 31 $ to (30 :: Integer)
False
> 30 $ to (30 :: Integer)
True
> 7 $ to (30 :: Integer)
True
-- Intersection
> intersection (interval (10 :: Integer) 20) $ interval 18 30
Interval {ivFrom = LowerBound (Finite 18) True, ivTo = UpperBound (Finite 20) True}
> contains (to (100 :: Integer)) $ interval 30 80
True
-- This means the interval between 30 and 80 is fully contained within -inf to 100
> contains (to (100 :: Integer)) $ interval 30 100
True
> contains (to (100 :: Integer)) $ interval 30 101
False
> overlaps (to (100 :: Integer)) $ interval 30 101
True
-- Because the interval does in fact overlap the range -inf to 100, however
> overlaps (to (100 :: Integer)) $ interval 101 110
False
-- as there is zero overlap
</code></pre>

*Note: this stuff takes a long time to digest and make notes on...*

# References

<a id="1">[1]</a>
Plutus Development Team, 2021.
plutus-ledger-api-0.1.0.0: Interface to the Plutus ledger for the Cardano ledger. Plutus.V1.Ledger.Contexts, section: Pending transactions and related types
<https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html>

<a id="2">[2]</a>
Manish Jain and Constantinos Dovrolis. 2004. Ten fallacies and pitfalls on end-to-end available bandwidth estimation. In Proceedings of the 4th ACM SIGCOMM conference on Internet measurement (IMC '04). Association for Computing Machinery, New York, NY, USA, 272–277. DOI: https://doi.org/10.1145/1028788.1028825

<a id="3">[3]</a>
Learn Cloud Native Development Team, 2019.
The Basics - Fallacies of Distributed Systems
<https://www.learncloudnative.com/blog/2019-11-26-fallacies_of_distributed_systems>

<a id="4">[4]</a>
Plutus Engineering Team, IOHK.
plutus-ledger-api-0.1.0.0: Interface to the Plutus ledger for the Cardano ledger.
<https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html>

# Footnotes

<a id="ft1">1.</a> The description is that of a pending transaction. This is the view as seen by validator scripts, so some details are stripped out. [[1]](#1)

-

# OLD NOTES CAN DISREGARD:

the process verifying the chain of ownership by passing the appropriate keys to the validator 


in order to unloc a script address - Tx to run - 3 paras: datum, redeemer, context

Data can be used

but we need to use types:

* Data and Redeemer can be custom types, so long as they implement the IsData type
* Context uses ScriptContext

So far we've only looked at the data and redeemer and ignored the context:

Example:

	mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
	-- we can add a condition when we're creating the validator, in this case: x must == y
	mkValidator () (MyRedeemer x y) _ = traceIfFalse "Wrong Redeemer" $ x == y

However, we have ignored the context...

In this lecture we're looking at the context

Plutus-Ledger-api

This is a package we didn't neccesarily need - but it's been included in this weeks cabal file

module-plutus.v1.Ledger.Contexts



Data Type = Typed Version (Custom Types As long as it 

third argument = ScriptContext


Datum

Redeemer

Context: plutus-ldger-api

