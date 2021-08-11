# Git: A Brief Introduction

> "My name is Linus Torvalds and I am your god." <br />
> — Linus Torvalds

*Note: Although Linus did put a lot of work into creating one of the most widely adopted, open source operating systems to date. To his credit, he doesn't actually take much of it when discussing his contribution to the development of git<sup><a href="#fn1">1</a></sup>.*

For more *interesting* quotes by Linus, see [this wiki page](https://en.wikiquote.org/wiki/Linus_Torvalds).

### 1. Introduction

This document is meant to be a very brief introduction to git, specifically how to use git in combination with the [Plutus Pioneer Course]() [[1]](). For super advanced techno(logical) vikings, I will include some links to additional references at the end (or perhaps a forum where you can go to argue about things would be more appropriate, and more inline with what I like to call: 'the Linus style' *— tongue-in-cheek*).

### 2. What Is Git?

Firstly, I think there is often a misunderstanding in relation to what Git actually is. It is important to recognise that **(at the very least)** Git and GitHub are two different things. The same comparison can be made between Git and BitBucket or *(to a lesser extent)* Git and GitLab.

To provide a quick explanation, Git is a set of algorithms and data structures that allows for peer-to-peer software development and collaboration. It is a way of sharing a software project amongst many people who can contribute to it. 

*An interesting thought experiment for anyone new to Git and open source software development: how can we stop malicious actors from introducing bugs and backdoors into software for their own personal gain?)*

It is interesting to note that whilst Git was developed in the mid 2000s - it does use what is essentially a Blockchain (or at the very least a 'linked-list-esque' data structure, which is similar to a Blockchain) to manage software projects. Whilst, GitHub (and other similar services) are platforms which host a Git repository (for all intents and purposes, you can think of a repo as the ledger, but in the simplest terms, it's the codebase) for you. These services are centralised, but it should be noted that you can maintain a git repo (project) through the use of peer-to-peer network communication.

At this juncture, it seems appropriate to make a comparison between distributed ledger systems, blockchain consensus algorithms and Git. Git is essentially a distributed ledger, everybody should have (if they have pulled down the latest code) the same copy of the codebase. However, the primary difference here is that there is an incredibly low probability of Byzantine actors [[2]](#2) due to the implementation of the peer-to-peer web of trust model [[3]](#3).

### 2.1 The Web Of Trust Model 

Instead of having to deal with a distributed system where the Byzantines Generals Problem [[2]](#2) is a very real threat, you may modify 'the system' to use an alternative security model that minimises threat (the system itself may not require a large degree of security considerations, although I wouldn't ever make that assumption).

In this case, you only allow access to the codebase to immediate trusted parties (say, you have six developers who you know you can trust), in turn they also have their own trusted parties. Since nobody really has anything to gain from acting maliciously (within a *potentially* trusted group of individuals — *never underestimate the power of human incompetency*), the probability of a Byzantine actor tends towards zero. Thus, Byzantine Fault Tolerance [[2]](#2) isn't something you really have to worry about. Therefore, it follows that consensus (everybody has the same copy of the codebase) is quite easily reached without the use of immediate monetary incentives.

*Note: since this document is contained within a repository about, for lack of a better word, cryptocurrencies, it seemed appropriate to draw some correlations between Git and DLTs + Blockchain Consensus Algorithms.*

### 3. The Typical Case

The way in which open source projects (and private projects ran by private companies) are *typically* developed is through the use of a service such as GitHub. 

*Continue Writing This At A Later Date...*

### References

<a href="#1" id="1">[1]</a> 
The Cardano Group, ADA. (2021).<br />
Plutus Pioneer Pprogram by IOHK.<br />
Available at: <https://testnets.cardano.org/en/plutus-pioneer-program/> <br />
Accessed: 10/08/2021

<a href="#2" id="2">[2]</a> 
Lamport, L., Shostak, R. and Pease, M., 2019.<br />
The Byzantine generals problem.<br />
In Concurrency: the Works of Leslie Lamport (pp. 203-226). <br />

<a href="#3" id="3">[3]</a> 
Baltakatei. (2020). StackExchange. <br />
The Web Of Trust Model.<br />
Available at: <https://crypto.stackexchange.com/questions/80629/what-is-the-pgp-web-of-trust-strongset/> <br />



### Footnotes

<a href="#fn1" id="fn1">1.</a> Believe it or not, Linus can be quite humble! See the below quotes. 

> "I maintained Git for six months, no more, ... The real credit goes to others. I'll take credit for the design." <br />
> — Linus Torvalds, Open Source Summit Europe

<div></div>

> I'd also like to point out that I've been doing git now for slightly over two years, but while I started it, and I made all the initial coding and design, it's actually been maintained by a much more pleasant person, Junio Hamano, for the last year and half, and he's really the person who actually made it more approachable for mere mortals. Early versions of git did require certain amount of brainpower to really wrap your mind around. It's got much much easier since. There's obviously the way I always do everything is I try to do everybody else to do as much as possible so I can sit back and sip my Piña Colada, so there has been a lot of other people involved, too. <br />
> — Linus Torvalds, Tech Talk: Linus Torvalds on Git at Google

You can watch the Tech Talk [Here](http://www.youtube.com/watch?v=4XpnKHJAok8).