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
 
-

