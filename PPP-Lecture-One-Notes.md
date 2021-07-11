### Notes on The Unspent Transaction Output Models (UTxO) and the proposed extensions by IOHK (Manuel Chakravarty, James Chapman, Kenneth MacKenzie, Orestis Melkonian, Michael Peyton Jones, Philip Wadler)

*Note: These are rough notes, will be tidied up when possible. Currently half way through Lecture One of PPP.*

* Transactions on the Blockchain that have not yet been spent, are unspent transaction outputs.
* An example of two unspent transaction outputs:
* Alice and Bob, Bob has 75 ADA, Alice has 25 ADA, Bob Wishes To Send 10 ADA to Alice.
* Therefore Alice creates a Transaction (Tx).
* Any Tx has any number of inputs and any number of outputs.
* When creating a Tx, you must use complete UTxOs.
* For any Tx, the SUM of the inputs must equal the outputs.
	* Exceptions:
	* Transaction Fees (Input > Ouput)
	* Native Tokens: Minting or Burning = Inputs -> or Outputs <-
* Your wallet is typically made up of many UTxOs

### EXTENDED UNSPENT TRANSACTION MODEL

* As (E)UTxO is a model that has limited scope, it's much safer and also it doesn't require knowledge of the entire system (blockchain) to execute. By having limited scope, there are a lot less problems that can go wrong, and it's a lot easier to verify whether or not a transaction can be confirmed easily.
* It's mathematically provable to demonstrate that Plutus is just as 'good' computationally as ETH, whilst being safer (and I imagine transaction throughput would be faster...? But thats just conjecture on my behalf).
* In ETH, your transcation may fail, but you'll still have to pay GAS fees. In Cardano, that simply won't happen, as it's computationally provable whether or not a transaction will verify before placing it on-chain.
* IMPORTANT: given the redeemer, the inputs, the outputs (of the transaction) and the dataum; the script can evaluate whether or not it's okay to consume the output (of the transaction).
 
### WHO IS RESPONSIBLE FOR PROVIDING THE SCRIPT, REDEEMER AND DATUM?
 
 In Plutus, it's the spending transaction has to do that; whereas the producing transaction only has to provide hashes (hash of the script and the hash of the datum that belongs to the transaction). OPTIONAL: can include script and datum FULLY.

### Getting up and running with the development environment

*Note: still working on this, written haskell in university - Manchester, Edinburgh, some experience, but reminding myself with the required tooling is going to take some time. For me, downloading haskellstack, compiling programmes with GHC, that's all okay - but dependency management, I'll need to add an extra 20 hours per week to catch back up to where I need to be... Which is doable, right now.*

I've been compiling basic Haskell programmes:

	main = print $ map (+125) [1, 2, 3]
	
Resulting in:

	[126, 127, 128]
	
Very basic stuff; but I need to do some extra reading on the additional tooling required, and pulling in new dependencies each week, it's going to be hard to keep up, but I'll be doing my best. Worst comes to worst, I'll concentrate mainly on the concepts and get to the implementations of the homeworks when possible.

-

*Authors Notes: But so far, the (E)UTxO model makes a lot of sense. I believe the implementation of smart contracts with limited scope is the only real way to implement a highly reliable, fairly easily testable and generally attractive system for adoption. Haskell is a good choice of language (it's safe, in fact - I remember watching Simon Patyen Jones taking the piss out of it a good few years ago when I was in uni, "a black box that turns on and heats up" - thankfully, it has a little more utility at this point). I would say that everything is looking good for Cardano - but as far as what has been successfully implemented, there is a way to go, a lot of work to be done. That being said, taking a methodical approach to writing academic papers, having them peer-reviewed, the unique approach being taken here is one that requires a high degree of merrit to succeed. So far, it's looking good.*

-

### HOMEWORK

- Clone Plutus-Pioneer-Repo [COMPLETE]
- Get it to build with Cabal [COMPLETE] 
- *Note: although, I am also taking other haskell refresher courses, I have been able to compile basic haskell programmes.*
- NIX [COMPLETE] - *note: rather a lot of dependencies...*
- PLUTUS PLAYGROUND [COMPLETE]
- SIMULATE AUCTION SCENARIOS [COMPLETE]
