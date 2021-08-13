# Lecture Six

> INSERT QUOTE HERE

### 1. Introduction

* Case Study
* How Can We Create A Complete App / Executable / Front-End / DApp / Test-Net / Mock-Chain
* All comonents required to deploy on mainchain, when time comes
* Oracles: service / way to get real world info on to the blockchain and make it usage in SCs
* External sourses: weather data, election results, time, temperatures, water levels, etc (what you would typically use an API for)
* Example: oracle required for real world betting for smart gambling smart contracts
* Simple approach
* One trusted data provider - one feed of data: ADA -> USD (exchange rate)
* You have to trust the data source
* Mitigation of risk (how about data aggregation)
* You could have trusted party put down collateral 
* You could take medium / average of data aggregate
* For example: 1 data provider we trust, real time data to Blockchain
* Represent data feed as a UTxO
* That UTxO sits @ script address, data field = current data for that oracle
* Example: Datum = 1.75 (it's an *unspent transaction* **OUTPUT** ... remember!? so of course, as it is a smart contract, it has a datum field. TxInfo [(DatumHash, Datum)]
* First problem: validation occurs when you want to consume a UTxO, not when you want to produce an output at a script address (hmmm, why couldn't you create a state machine, so a validator which takes the context -> txInfo::Datum ... CONSUME <- this then equals a new UTxO, assuming you have some unspent output, so you'd pay ADA to the script address, the redeemer would evaluate the TRUE, you could consume the UTxO as you grab the data from the datum field, then I assume you would have written the off-chain code to add new Datum to the UTxO...? This is just my intutition, I've paused the video. I think having a guess at how an implementation may be applied before continuing and hearing how it actually works is constructive).
* Ahh, I didn't think about implementing an oracle as an NFT, that does in fact make sense. If you own the only minted NFT, which is the oracle, there can only be one oracle in existence, for your purpose, at that address.
* Oracle carries output: Datum, NFT
* Oracles are a bit meta, you don't know how people might want to use the data feed, kind of like an open API.

<hr />

***Again - focusing on listening to lectures and getting work done, will return to lecture notes.***