# Blockchain Lectures Notes (BLN) - README.md

*The Second Cohort Of The Plutus Pioneer Programme*

### 1. Authors Note

I have made the decision to begin making notes and documenting all lectures and exercises found within the Plutus Pioneer Program (cohort two). The writing style will be two-fold: technical, but creative. This is for numerous reasons. Firstly, writing does aid the cognitive ability to solve problems [[1]](#1), Plutus is a new platform, Plutus-core is a new language, Haskell is somewhat unique and understanding aspects of how this whole system fits together requires an element of creative thinking and problem solving. Similarly, these technologies <sup>[1](#fn1)</sup>. are novel in nature, as such, it may be required to do some creative writing in order to explore new potential ideas.

I've been using distributed ledgers and blockchain consensus algorithms for over ten years. I remember buying BTC at \$3 and seeing it hit \$7 was mind blowing for me (at the time). It's a shame I never held on to many, but I just look back and laugh. Why? Because it's not about money, although most people would disagree (this is one of the reasons why I feel somewhat at home within the Cardano community). It's about technology and ultimately: changing the world. It's a strange thought: we are individuals, we have choices, and if we decide to do so, we can make a real difference here. If there is anything worth pursuing, it's making a difference. This is why I decided to devote as much time as possible to the difficult task of participating within the Plutus Pioneer Program and brushing up on Haskell.

I would like to express my gratitude to IOHK for constructing this program and accepting me as a candidate. Furthermore, I would like to thank my family for their continual support in my efforts to pursue a career path which isn't thought to be conventional.

I believe I can speak for the entire community when I extend my sincerest thanks to the world class engineers and scientists who have contributed to building Cardano as of current. Charles, you will are and will continue to be an inspiration to us all and to the world. I hope we can all collectively strive to make positive societal change.

### 2. Preface

This set if notes, written in a casual, formal, yet creative style has been cultivated through the activity of participating within the second cohort of the [Plutus Pioneer Program](https://testnets.cardano.org/en/plutus-pioneer-program/). I felt it to be important to document my own experience with the program in order to gain the most from it. Furthermore, it is my hope that the notes themselves (and perhaps even through the contribution of others) will be able to help other aspiring developers for time to come.

To be completely honest, I'm not hugely qualified to write any of this! Which is why I would hugely appreciate any corrections via contribution. I do however hold the following qualifications:

* A First Class Honours Degree in Computer Science from the University of Manchester.
* A Postgraduate Diploma in Artificial Intelligence / Informatics (I hope to write my thesis on this very subject after COVID-19 has 'blown over', thus obtaining my MSc) from the University of Edinburgh.

*Note: for what it's worth I hold a good few years experience in industry writing web applications... Not exactly what you would call academic.*

I wrote this set of notes in an ad-hoc style manner. As the lectures were released, I've done my best to keep up with work, in addition to following the lectures, implementing the homework's and keeping these detailed notes.

### 3. Foreword

*To any and all IOHK employees: feel free to contribute by adding a foreword via a pull request.*

### 4. Formatting

As these are technical notes, the format for each set of lecture notes will be written to a pseudo-technical specification. This means each set of notes should follow a template, making it easier for readers to break down and understand. Each set of notes will be presented in accordance with the following schema:

* Introductory Lecture Information
* Incremental Sections (1. → N.)
* Subsections (1.1 → N.X)
* Possible *Coffee Time* Sections — Thought Experiments, or Exercises (These Will Be Italicised) and will be specified as 'COFFEE TIME!'
* End Of Section Notes — Optional, Only If Required
* Exercises and Associated Comments
* Questions and Comments on Possible Problems (Things I May Be Uncertain Of)
* Lecture Summary

I will be doing my best to keep to the above schema for each set of notes, but this isn't a publishable book or formal technical documentation, so expect a degree of variance in style.

*Note: both footnotes and references will be used within this document set, which may seem confusing at times. However, please be aware that footnotes are formatted as enumerated superscript values, whereas footnotes are enumerated bracketed values.*

*Quotes: You will see various quotes throughout the text, this is simply how I like to write.*

### 5. Content Within This Repo

The contents of this repo is two-fold. Firstly (and most importantly), the content is a set of pseudo-academically written notes for the [Plutus Pioneer Program](https://testnets.cardano.org/en/plutus-pioneer-program/) [[3]](#3), which follow the formatting as outlined in §5. Secondly, a set of academic papers which I have deemed to be of greatest importance **to me** is also provided. In addition, these papers will (slowly, but surly) be printed out, annotated and scanned. Shortly thereafter, these scans will be re-uploaded to this repo.

This repo will **forever and always** remain open, unless instructed otherwise by a member of the Cardano organisation or by an employee of IOHK.

### BLN: Intended Audience

These notes are for anybody and everybody (somewhat vague, I know), but the audience could range from those who are current Plutus Pioneers, individuals who are just following the programme, developers from other communities, or even legislators (however, I highly doubt they would be reading my notes / documentation on Cardano). But ultimately, we are all here for the same reason, we believe in what we're building and the best way to approach adoption is to allow everybody to see what you're doing, at least, that would be my intuition.

But, to reduce the scope a little bit and be somewhat more realistic, the intended audience is really for those who are looking to develop on Cardano.

### BLN: Why These Notes May Be Helpful

*In Progress*

### BLN: Contributing

*Anyone is free to contribute via a pull request.*

### BLN: Table of Contents

 - [Lecture One](1-Lecture-One.md): BTC: UTxO and Cardanos' (E)UTxO Model, Local Plutus Playground, Compiling Smart Contracts
 - [Lecture Two](2-Lecture-Two.md): (E)UTxO Catch Up, Redeemers, Context, Datum, Data Type: Data, More Smart Contracts
 - [Lecture Three](3-Lecture-Three.md): Problems in the Playground, EUTxO Refresh, TxInf & ScriptPurpose, On-Chain, Off-chain, Time, Examples
 - [Lecture Four](4-Lecture-Four.md): Monads, The EmulatorTrace Monad, Contract Monad, Homework
 - [Lecture Five](): -
 - [Lecture Six](): -
 - [Lecture Seven](): -
 - [Lecture Eight](): -
 - [Lecture Nine](): -
 - [Lecture Ten](): -

*Note: Currently Writing Notes On All Lectures Marked With '-'*

### BLN: Summary

*In Progress*

### BLN: StackExchange (Cardano) Answers:

1. [What exactly is a redeemer?](https://cardano.stackexchange.com/questions/471/what-exactly-is-a-redeemer/2208#2208)

### BLN: References

<a id="1">[1]</a>
Kellogg, R.T., 1999.
The psychology of writing.
Oxford University Press.

<a id="2">[2]</a>
Jones, M.P., 2021.
Plutus Tx: compiling Haskell into Plutus Core. Viewed 2nd August 2021.
<https://iohk.io/en/blog/posts/2021/02/02/plutus-tx-compiling-haskell-into-plutus-core/>

<a href="#3" id="3">[3]</a> 
The Cardano Foundation. 
IOHK. 
Last Updated: Early July 2021. 
<https://testnets.cardano.org/en/plutus-pioneer-program/>


### BLN: Footnotes

<a id="fn1">1.</a> The technologies I am referencing are Distributed Ledgers and Blockchain Consensus Algorithms — 'cryptocurrencies'

### BLN: Nomenclature

*In Progress*

### BLN: Appendix

*In Progress*

### BLN: TODO

1. Correct any errors within any of my current lecture notes.
2. Continue to include references and footnotes within all lecture notes.
3. Finish the *In Progress* sections within this README.md.
4. Catch up to and finish Lecture Seven ASAP.
