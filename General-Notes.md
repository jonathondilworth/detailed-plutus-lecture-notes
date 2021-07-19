***General Notes, lecture one note being compiled and organised properly...***

-

Coffee Time!

*Coffee Time: Plutus Core, being on-chain, is what is ran when a transaction is executed by the network.*

*Coffee Time: Plutus is the platform built to execute smart contracts on Cardano, whilst Plutus-core (which is essentially Haskell) is the language used to write smart contracts for Plutus.*

*Plutus Smart Contracts are Haskell Programs. However, you won't ever write Plutus-core code, you'll write Haskell code which compiles into Plutus-core, to then be executed as a transaction on the Plutus platform.*

*Plutus-core is essentially System F Omega with Recursive Types.*

*Plutus-core is similar to an assembly language (you use a higher level language to compile down to Plutus-core, in this instance, we're using Haskell, but I'm sure attempts will be made to build compilers that can take other languages and compile them to Plutus-core, which is essentially System F Omega with recursive types. Everything you can implement in Haskell, you can implement (in a more verbose style) within System F Omega, but there are some elements of SFO which do not exist in haskell.*

*In Cardano's case Plutus-core is implemented 'on-chain' (it's executed by operational nodes within the Cardano network infrasturture. You would ever typically write Plutus-core, rather, you would write Haskell (currently) which would compile to Plutus core. At which point, you have an Application (or DApp) which can be tested locally and deployed to the network.*

*When implementing a Plutus smart contract or application, you: 1) write a Pltus core compilable language (currently, this is Haskell, because both Plutus-core and Haskell are instances of System F Omega; however, some clever people implemented recursive types in System F Omega. Note: Haskell does not support all components implemented by System F Omega, and vice versa; so you will be prompted at compile time if you're doing something nauty!*

*Note: Sytem F Omega with Recursive Types IS Plutus-core*

**Interesting Point: State Syncronisation: Double Spend**

* Backup
* DevOps
* Monitoring
* Logging

* Rollbacks in Cardano: As far as I can tell, Rollbacks (failure to validate a transaction within a series of transactions, which invalidates a portion of the Tx within the series) occur when simultaneous block creation occurs. The longest chain (or series) of transaction will always be chosen to represent the true state of the Blockchain. This is dangerous for DApp developers and non-deterministic implementations of smart contracts, because people may loose their money, or native tokens.


> "Constraints Liberate, Liberties Constrain"

— Runar Bjarnason

* Similarly, you could say: Less is More, inofar as: if you trancate the scope of an application, the less you allow it do, the easier it is to control. Thus, it follows: the less likely it is to cause catestropic failure. Furthermore, it allows for an increased capacity to predict states and behaviour given any set of constraints.
* Designing applications such that they're "deterministic, pure, replayable, sandboxed state machines" (Michael Peyton-Jones) means as developers we're much less likely to introduce bugs into our applications (*possibly* mathematically provable).
* 

Must be

Nomenclature:

* Plutus-Platform: The 'on-chain' and 'off-chain' portion of Cardano's smart contract implementation. This faciltates smart contract transactions using off-chain code (implemented in Haskell, note: it desn't require compiling to Plutus-core). Off-chain code then performs transactional validation checks to ensure such any such transaction ... or smart contract?? ... can be legitimately executed. While on-chain code performs the execution of any validated transactions via nodes on the network (when prompted to) and writes to the Blockchain accordingly.
* Plutus-Core: The 'Assembly'-esque language (essentialy System F Omega + Recursive types) that is used to implement smart contracts as potential transactions that may be executed on the Cardano blockchain (so long as they're validated off-chain and somebody actually interacts with the contract at a given address). Contracts can remain 'stale' forever (if nobody changes their state by executing a transaction at that address.
* Plutus-Tx: The Compiler which takes a subset of Haskell, compiles it to 'Plutus-core', at which point, it can be written to the blockchain at an address. Note: it will not compile if the 'off-chain' Haskell fails to verify the ability to perform the transaction, 

 in this instance it is implemented 'on-chain' and it should be noted that nobody is expected to write Plutus-core. As System F Omega 

The Plutus Pioneer Course aims to provide:

* Most obiously: a thorough introduction to Plutus.
* The underlying concepts on which Plutus operates.
* An overview of Smart Contracts on Cardano (currently limited to Plutus' implementation via Haskell, although, there will be oppurtunities in the future to implement these contracts using alternative languages, such as Solidity).
* The course introduces core testing principals and techniques for Smart Contracts.
* The ability to perform offline local testing will be demonstrated.
* A deep dive into Native Tokens on Cardano
* minting and burning of native tokens
* deploying plutus contracts and writing backends for plutus contracts

NOTE: Plutus learning is not easy:

INSERT REASONS.

EUTXO

* Less Intuitive Than ETH Accounting Model
* Plutus is brand new and is still under rapid development.
* During the course, Plutus will undergo changes which will likely break compilation between the beginning and end of the course (Syntax Changes).
* Tooling has it's issues (as the product is so early in development. 
* documentation is almost none-existant (currently).
* Compiling Plutus may be difficult.
* It is suggested that NIX is likely the best way to compile Plutus.
* However, the senior development team are planning on creating a docker image for a Plutus-centric environment, which **should** make things simpler.

* Plutus essentially is Haskell (Haskell compiles into Plutus-core), so you are writing Haskell when implementing a Plutus DApp or smart contract.
* If you're unfamilar with Haskell, it's going to make things difficult. If this is the case for the reader, it has been suggested that you spend at least 400 hours (40 horus per week, for 10 weeks) learning the language and its advanced features (as these language features are implemented within Cardano DApps and Smart Contracts). 
* [TODO: find good resources for Haskell learning]
* Haskell Programming is quite different to what you may have learnt in other jobs or within University (although, it's often taught within mathematically-centric modules).

* Our team is built of engineers from various backgrounds, but Plutus is a new technology. If something goes wrong it's not as simple as checking stackoverflow for a quick solution. This is, however, exciting, as our team is the first set of individuals on the planet to use Plutus (which is likely one of the best implementations of smart contracts on distributed legers that has ever been implemented).

### Introduction



### Cardanos' Ledger

* (E)UTxO Model
* Native Multi-asset Support
* Plutus Core (see §x.x)

### "And God Said: Let There Be (E)UTxO"

Firstly, we need to introduce UTxO (as implemented by BTC).

##### UTxO

* UTxO stands for Unspent Transaction Output.
* Transactions are functions which are applied to a Blockchain data structure.
* Fundamental Concept of the Ledger: Transactions have outputs with 'money' (digital tokens) in them. In the case of BTC, the outputs were represented by numerical values of quantities of BitCoin.
* If you want to get money, you have to consume an output (from a previous transaction) and then you produce more outputs. It's perpetual in nature.

**Restricting 'people' from spending ANY output from any given Tx**

* To put it simply (if you have a degree in computer science with a focus on cryptography!), you can use a public-private key implementation to lock the outputs of a Tx, as each output has a public key (which, in BTCs case, acts as the address).
* It does get slightly more complicated, as the implementation in BTC uses a very basic scripting language to implement this (so, technically, BTC does have smart contracts, but they may as well be called dumb contracts).
* In BTCs case (or any DLT/Blockchain that utilises UTxO) you have a validator and a redeemer. The validator is a basic script which takes the redeemer as an argument, and returns a boolean value which either enables or disables the ability to initiate another Tx using the unspent output of the previous Tx.

##### Cardano: (E)UTxO









[FOOTNOTE: if that sounds confusing, essentially, you're running the set of outputs individually through a cryptographic algorithm - likely RSA or ECC - to generate a public key and a private key, the public key is, in BTCs case, an address that the BTC is stored at, and the private key is the only way of decoding data stored at that public address. So, if you loose your private key, you loose your money. However, it gets a little more complicated... To spend the money, so to initiate a Tx, you are required to sign the next transaction with the private key that only you know, and it must have come from the owner of the BTC at the address on-chain, because the address is the public key, and a digital signature can be generated by encrypting the public key with the private key, so when another user - in this case, a script - runs the output of the signature through the public key, it results in the public key, so its provable that the funds came from that address this demonstrates that an individual with the private key to that address has initiated the transaction]

##### What is (E)UTxO?

-













