# 3. Lecture Three: Introduction (In Progress - currently in random note form!)

*I take notes whilst listing to the lectures and doing the exercises / homeworks, then I format them nicely.*

**This Document Is Under Construction!**

### Introduction

* Playground Memory Issues (Hardcoded Timeout)

##### 1. Playground Memory Issues

You are now able to modify the timeout in the Plutus Playground Server (located within the Plutus Playground Client folder) to any number of minutes. This is accomplished by running the following command:

	plutus-playground-server -i 120s
	
Note that you can modify the value of 120s to any amount, this will set the timeout in XXX seconds <code>XXXs</code>.

-

##### 2. Quick Refresh On The (E)UTxO Model

(E)UTxO stands for **Extended Unspent Transaction Output Model**.

*Let me just bring you back up to speed on UTxO...*

This model is similar to how BitCoin (BTC) manages unspent digital assets held by any given wallet (*these assets can be thought of as money...* in BTCs case, these assets would be BitCoin). However, with BTC, the model is simply built through very simple UTxOs (unextended).

Given a simple UTxO, there is a **redeemer** and a **validator**. In order to spend any unspent transaction output, the redeemer can be thought of as a cryptographic key which is passed to the validator. Once any given UTxO is attempted to be spent, the validator is then responsible for verifying the chain of ownership by using the redeemer to verify that the UTxO belongs to whichever wallet is attempting to create a new UTxO.

***With an (E)UTxO model, you have a script at a given address, a redeemer, context and datum...*** Validation must still occur in order to spend any given (E)UTxO. However, the script address is contains a reference to a set of instructions. The instructions **(the script)** contains arbitrary logic, some of which is responsible for creating a validator (plus a possible set of numerous constraints under which the validator may or may not verify the chain of ownership of any given (E)UTxO).

Furthermore, the (E)UTxO model has the ability to facilitate ownership of multiple types of asset (for example non fungible tokens can exist within a (E)UTxO model. Even the ownership of other coins can be transferred from one wallet to another given the EUTxO model).

##### 2.1 EUTxO Scripts

EUTxO scripts are held at a script address. During lecture two we saw a low level implementation of a validator within a script where all three arguments were defined as the Haskell type: <code>Data</code>:

<pre><code>mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()	
validator :: Validator
validator = mkValidatorScript $\$(PlutusTx.compile [|| mkValidator ||])
</pre></code>

In practice this is **not used**. We instead use the typed version. This is where data and redeemer can be custom types, as long as they implement the <code>IsData</code> type class. The third argument (the Context) is must be of type <code>ScriptContext</code>.

In the examples we have seen so far, we've only been examining the data and the redeemer.

-

# OLD NOTES:

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

