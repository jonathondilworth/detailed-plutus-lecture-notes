# Lecture Five

>"It might be true that there are six billion people in the world and counting. Nevertheless, what you do makes a difference. It makes a difference, first of all, in material terms. It makes a difference to other people and it sets an example." <br />
> ― Robert Solomon, Waking Life

### 1. Introduction

*Currently Being Written*

<hr />

**ROUGH NOTES**

As is per usual, we're going to need to pull in the appropriate code for this weeks lectures and exercises.

*Note: some of you whom are more familiar with Git than others can skip over this, but I have noticed an up-tick in the amount of git-related questions on the IOHK Discord and sometimes, yes, you do get problems with it. I remember being a junior dev straight out of university and spending a good amount of time trying to understand what exactly what a detached HEAD was. It sounded like something out of Final Destination (the film). Anyway, all kidding aside, I will place a link here to a basic Git introduction and follow it up by a few links to external resources you can continue to learn from, if required.*

<hr />

* No other native tokens apart from ADA from genesis -- no minting / burning
* Addresses & Values (UTxO), EUTxO: Address (Script Pointer), Value, Datum
	* Set { Inputs }, Set { Outputs } + Validator, Redeemer, Datum & Context
* Relevant Types: Plutus-Ledger-Api
	* Plutus.V1.Ledger.Value
	* Plutus.V1.Ledger.Ada
* Value :: Map CurrencySymbol (Map TokenName Integer) -- ByteStrings Represent a Coin (AssetClass)
* Native Tokens are identified by CurrencySymbol & TokenName
* CurrencySymbol is simply a New Type Wrapper around a ByteString
* Ada will be its own AssetClass, whilst other Native Tokens are their own.
* A value = how many units of each asset class are contained within
* import Plutus.v1.ledger.Value
* import Plutus.v1.ledger.Ada
* Override xOverloadedStrings
* :t adaSymbol
* :t adaToken
* :t lovelaceValueOf
* lovelaceValueOf 100 -- produces a value of 100 lovelace
* We can combine values:
* lovelaceValueOf 10 <> lovelaceValueOf 100 -- value: 110 lovelace
* :t singleton 
	* singleton :: CurrencySmybol -> TokenName -> Integer -> Value
* singleton "a8ff" "ABC" 7 -- token called ABC, of value 7, with a8ff symbol
* What goes up must come down, what goes in must come out (except for, of course: the fees!)
* This is why we need a minting policy (which is why I assume we get a policy ID? a function of the entire minting process maybe? Like a hash(mint)?
* Fees depend on script memory use + number of instructions and size of transaction in bytes (not in value)
* YES! I guessed it correctly! Lecture five is so much more fluid than lecture four, lecture four felt like part of a Haskell course, it's fairly intuitive, I'm just kicking myself for spending so much time documenting progress.
* Cannot mint or burn ADA because there hash(<empty>) is non-sensical, I suppose you could say hash(<empty>) is null (I suppose, the Haskell word would be: nothing).

<hr />

These notes take AGES to compile. I'm going to crack on with the lectures for a while, then return to making these notes.