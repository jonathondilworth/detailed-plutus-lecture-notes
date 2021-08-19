# Lecture Six

> “Unless you try to do something beyond what you have already mastered you will never grow.” <br />
> — Ralph Waldo Emerson

### 1. Introduction

*Note: You always write your introduction last.*

### 2. Oracles

> "Know Thyself." <br />
> The Oracle, The Matrix <br />
> It would be disingenuous not to also attribute this quote to Socrates

For a moment consider applications in general. As we have already concluded, they're typically fairly 'unsafe' and can run off to do some IO without asking (on the odd occasion). Fortunately we're fairly decent programmers *(I think some circle jerking is fairly appropriate at this point in the course, kudos to all those who have stuck through it)* and our programs know how to behave. However, sometimes it is appropriate to accept data from a stranger, and **to be clear**, that's what oracles are. They're strangers, who knows how reliable some random fortune-teller you just met is?<sup>1</sup>

Unfortunately this untrustworthy bunch are the only people who seem to know anything around here. So, that's where can get our IO from the outside world from. Now, there are mitigation schemes. Lars does go into this within the video, so I won't spend too much time discussing them. But we're looking at:

* An Oracle putting down some collateral, which incentivises this 'randomer' to actually be truthful, otherwise: bad things happen to them (they loose their money).
* You could aggregate many different oracles together and take the mean? If there were a number of oracles outside an assigned threshold, the transaction could abort / rollback.
* Stuff like this... c'mon guys, start thinking creatively!

### 2.1 What have we learnt thus far?

We now know that:

* Oracles make data (similarly to OPEN APIs) accessible to smart contracts within the eco-system.
* Oracles are not to be trusted<sup>2</sup>.
* Oracles are ONE way of getting data from the outside world into a smart contract, smart contract validators can be (as is the case with Haskell function, I think?) 'overloaded' with parameters (don't confuse the term overloaded with the OOP keyword, in this instance it's simply an adjective), meaning we could inject our own outside data via the redeemer, this would be suitable in cases where, for example, you need multiple signatories to access a smart contract (or an oracle).
* The value provided by an Oracle is an output from the Tx paid to the oracle, the particular output is stored within the datum.

### 2.2 Use Cases

Let's keep this short and sweet (I want to get developing on the #purple test net ASAP!):

* 'one-shot' oracles, where the value does not change over time (for example, the result of a football game) - betting.
* Distributing information about weather conditions to any vehicle that requests it.
* I mean, you be creative!

*Note: this is a SIMPLE APPROACH, you can do things such as aggregate data, process it, run it through functions, etc.*

### 3. How Exactly Do These Things Work Then?

Right.

>*COFFEE TIME. <br />
 Take five minutes, have a think - how could they work?
 It doesn't matter if you're wrong, remember: <br />
 "A foolish consistency is the hobgoblin of little minds" ...
 and "to be great is to be misunderstood" <br />
 I would highly recommend some Emerson if you've not read his work.*

Let's work through this methodically:

1. What are our requirements? What are our constraints?
2. We require a smart contract with two...

**Actually, this is going to take too long, I'll just explain. Sorry, I have the capacity to be a good teacher, I'm just trying to finish up quick so I can get to work on the test net.**

* Any wallet (that has been used) has a number of UTxOs
* Each Tx validated by the network IS UNIQUE
* This means you can select a specific Tx and iterate though the outputs.


### Footnotes

[1](#fn1). That's why a couple of us have a small pet project in the making, more to come on this soon!

[2](#fn2). See the following funny quote from Snatch by Guy Richie (if you've not watched it, do it!):
> What's in the car? <br />
> Seats and a steering wheel. <br />
> What do you know about gypsies? <br />
> I know they're not to be trusted. <br />




<hr />


<hr />

<hr />




***Again - focusing on listening to lectures and getting work done, will return to lecture notes.***

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