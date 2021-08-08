**Really Random Notes**

-

Additional Terms:

* Tx - Transaction.
* Distributed Ledger - You may have a ledger (not implemented as a Blockchain) in a distributed manner.
* Blockchain - A distributed ledger with specific properties.
* Consensus Algorithm - An algorithm where various parties with varying degrees of trust between them can maintain a high degree of Byzantine fault tolerance.
* Proof of Work - The use of physical resources to ensure cryptographic immutability of data within a distributed ledger (Blockchain).
* Proof of Stake - Various types of PoS algorithms exist, but the central idea is to remove physical resources and replace them with virtual resources.

-

Interesting Statements:

* Cardify finds that only 16.9% of investors who have bought crypto “fully understand” the value and potential of cryptocurrency, while 33.5% of buyers have either zero knowledge about the space or would call their level of understanding “emerging.”
* In order to change the state of the blockchain, there must be a new Tx that consume UTxO and create new ones.
* UTxOs will never spring into action themselves.
* Wallet logic (off-chain) can execute sophisticated logic.
* DATUM - OUTPUT STATE (can create state machines) - STATE OF THE UTXO [enables state machines?]
* CONTEXT - COULD BE VERY RESTRICTED (JUST REDEEMER, OR FULL BLOCKCHAIN | In Cardano it's just Tx+inputs+outputs).
* Plutus script gets 3 pieces of data: Datum (parameters for script), Redeemer (Input & Validation), Context (Tx, Inputs, Outputs).
* In Cardano, the Context represents the Tx, including all it's inputs and outputs.
* SCRIPT INPUT: Dataum, Redeemer, Context.