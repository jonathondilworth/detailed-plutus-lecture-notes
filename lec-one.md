# 1. Lecture One: Introduction

Within this set of notes the following information will be presented. Firstly, the courses administration details are provied. Secondly, a coverage of the course content is outlined. Shortly thereafter, details about Plutus, the difficulty of the program and the (pseudo-optional) pre-requisites are given. Furthermore, an extensive explaination of the (E)UTxO model (which facilitates the possibility of transactions on the Cardano network - **Cardanos unqiue accounting model**) is fully explained. In addition, building an example contract is presented, which includes how to setup a NIX shell, how to start plutus-playground-server and enable it to be locally accessible from the browser. Smart Contract compilation is demonstrated and an example of how to use the plutus platform in whole is shown. As pioneers we are then encouraged to try this ourselves.

## 1.1 Administration

- Lectures on every Thursday.
- Q&A Sessions Every Tuesday.
- Oaccional Guest Lectures.
- Use the Discord server (and slack) to help one another when possible.

## 1.2 Course Coverage

**The Plutus Platform:**

- Smart Contacts on Cardano via (E)UTxO
- Writing Smart Contracts with Haskell (Local, Production)
- Compiling Haskell to Plutus-core using Plutus-Tx
- Details Surrounding Plutus-core (GHC Plug-in)
- Plutus-Tx Compiling Details (GHC Core): Intermediary Languages / Intermediary Representations

**Further Underlying Concepts**

**Smart Contracts In Detail (and their various forms)**

**Testing and Implementing Smart Contracts**

- Using the Plutus Playground
- Offline Locally

**Native Tokens On Cardano**

- Minting Native Tokens
- Burning Native Tokens
- Use of Native Tokens Within Smart Contracts

**Deploying Plutus Contracts**

**Writing Backends for Plutus Contracts**

## 1.3 Plutus Platform

> "Plutus Platform Learning is Difficult" - Lars Brünjes

*Why So Difficult?*

* Using (E)UTxO Model - Less intuitive than other similar technological implementations (E.G. ETH Accounting Model).
* Plutus is brand new and under rapid development, thus is changing all the time.
* Due to constant changes, we're required to regularly update project dependencies.
* Tooling is not ideal...
* Difficult To Access Syntax & Repl Docs.
* Difficult to Build Plutus (It's likely best to use NIX)
* The Docker Image for the Plutus Platform is not yet ready.
* Plutus-core is compiled down from Haskell, Haskell is fairly difficult.
* It is reccomended that you spend 40 hours per week, for 10 weeks to gain a solid understanding of Haskell before or whilst undertaing this course.
* Plutus is BRAND new - we are the first people **ever in the world to write plutus code**.
* This means: no quick answers from stackoverflow or google...

However,

* Haskell courses and documentation has been made available to help pioneers learn.

# 2. The (E)UTxO Model

> “Any fool can know. The point is to understand.”
> — Albert Einstein [[1]](#1)

## 2.1 What is (E)UTxO?

(E)UTxO abbreviates: "Extended Unspent Transaction Output", but to understand the extended model, we must first examine the unextended model (UTxO, firstly implemented by BitCoin [[2]](#2)).

## 2.2 UTxO Explained

>"If you want to get money, you have to consume an output that is laying around and in turn you get more outputs."
> — Michael Peyton-Jones

UTxO, which, simply put, is a model of accounting and is used to identify how much 'money' (in this case: a digital 'currency') any 'wallet' [^1] on a blockchain data structure contains within it [^2]. Each wallet address on a permissionless blockchain such as BTC is associted to a set of cryptographic 'tools' which lock any funds (BTC, Digital Currency) that are tied to the above mentioned wallet. Spending funds (e.g. sending BTC from one wallet to another) equates to the transfer of ownership of BTC from one private key owner to another [[3]](#3). This is accomplished by signing the transaction with initiators private key, which can only be confirmed by verifying the signature using the initators public key. *Note: This is all accomplished through the use of a software wallet implementing transaction algorithms [^3].*



# References

<a id="1">[1]</a> 
Simmons, G.F., 2003. 
Precalculus mathematics in a nutshell: geometry, algebra, trigonometry. 
Wipf and Stock Publishers.

<a id="2">[2]</a> 
Antonopoulos, A.M., 2014.
Mastering Bitcoin: unlocking digital cryptocurrencies.
O'Reilly Media, Inc.

<a id="3">[3]</a> 
Nakamoto, S., 2008.
Bitcoin: A peer-to-peer electronic cash system.
Decentralized Business Review, p.21260.

<a id="4">[4]</a> 
Pardalos, P., Kotsireas, I., Guo, Y. and Knottenbelt, W., 2020.
Mathematical Research for Blockchain Economy.
Springer International Publishing.

# Footnotes

[^1]: The use of the word: wallet can be misleading (and this may be why UTxO models are not hugely intuitive). A wallet is simply the stored set of cryptographic 'elements' generated through the use of ECC Public-Private Key cryptograpy [[2]](#2) and hashing algorithms. In the case of BitCoin, a user generates a public-private key pair, and their 'wallet' address is generated using the following hashing algorithm: $ADDR = RIPEMD160(SHA256(K))$, where K represents their public key and is then shortened further using additional functions that you can read about [here](). These keys (or 'cryptograhic elements') are stored within their wallet, which, if they are managing themselves (and not through the use of a central exchange or central online wallet) are kept offline (and are always off-chain). However, wallet addresses (which are tied to a wallet computer program) are available on-chain (they have to be in order to initiate a Tx).
[^2]: Wallets can contain hundreds of public-private key pairs and the owner for X amount of BTC can in turn have hundreds of unspent transaction outputs. However, it must be noted that when such a user creates a new transaction, the set of their unspent transaction outputs will be used as inputs into the following transaction.
[^3]: Transaction algorithms such as: P2PKH: "Pay To Public Key Hash", P2PK: "Pay To Public Key", P2SH: "Pay To Script Hash", P2WPKH: "Pay To Witness Public Key Hash" [[4]](#4).