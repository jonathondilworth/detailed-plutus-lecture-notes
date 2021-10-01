# Programming In Haskell

> — Graham Hutton (Second Edition)

*I've been programming for years (which likely means I have a lot of bad habits). Thus, I'm by no means the best software engineer in the world, but I can usually pick things up fairly quickly. Guess what? I found picking up - technically Plutus - Haskell DIFFICULT. So, if you're learning Functional Programming for the first time I CANNOT RECOMMEND THIS BOOK ENOUGH (and I have only read the first chapter, it's as gentle as Blockchain Basics, if you've read that, I forget the author). Learning Haskell, or at university? Do yourself a favour and pick up this book. In fact, do you consider being a software engineer? Go ahead and drop, what, £30 on this book (I am in no way affiliated with the author/publisher, etc). I am just happy to find such a great resource. Even the foreword is beautifully written:*

> "John Hughes famously stated in his classic paper [*Why Functional Programming Matters*](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) that we cannot make a language more powerful by eliminating features. To that, we add that often we cannot make a language less powerful by removing features." <br />
> — Foreword, by Erik Meijer

In an instant, a phrase I heard Michael Peyton Jones state makes sense to me now:

> "Constraints Liberate. Liberties Constrain."

..or, something to that effect. Further, I do believe it is attributed to somebody else.

> "By the time [one] arrives at monads, every reader will feel that they themselves could have come up with the concept of a monad as a generic pattern for composing functions with effects."

There are a couple of points in the preface which should be highlighted, only that the book is split into two sections, the first being core concepts of the language, which we should be able to move through quite quickly (assuming my schedule isn't difficult to manage again...), followed by more advanced topics. Secondly, I feel this is an extremely important point, as I often allow myself to write **a lot** of notes (mainly because I enjoy writing, if I am honest), then reading much more code than writing. This is a problem, because Haskell is easy as hell to read, but you've got to write it! As such, the point I am highlighting within the preface is as follows:

> "It is vital to write Haskell code for yourself as you go along, as you can't learn to program by reading."

**I'm Sold. Let's Get Busy.**

*Right. Let's fly through some of this material and then catch up on some FP lectures.*

*These particular notes are more, for me.*

### Basic Concepts

#### 1.1, 1.2 Functions & FP

* Function is a **mapping** .s.t. arguments >= 1 and produces a single result.
* double x = x + x ... double (double 64) => double 128 => 256
* There are various ways of expressing the same result.
* Why FP? It's concise, it's nice; implement a sum using a loop in java VS sum [1..n] in Haskell.

#### 1.3 Features

* **Concise Programs** It's True!
* **The Type System** In Haskell is pretty interesting. As far as I understand: it's very well defined and 'strongly typed' **at compile time**. That is to say: you can write pretty much whatever you want (and it would parse as being legitimate Haskell, well, not literally anything, but you see what I mean... It would just fall down when it runs into any kind of semantic analysis performed by the compiler, or I guess, via the GHCi).
* **List Comprehensions** Which we all know and love.
* **Recursive Functions** f (x:xs) = x + f xs
* **Higher Order Functions** (passing functions as results and such) => DSLs (Marlowe).
* **Effectual Functions** Monads and Applicatives; so, the book outlines the reasoning as to why Haskell can be 'thought of' as a pure functional language - I was under the impression that, yes, it can be, so long as referential transparency is maintained? Essentially: determinism (within the programme, hopefully not the universe, there should be some room left for freedom, one would hope).
* **Genrics** We've seen these before, right?
* **Lazy Evaluation** Don't do computation if you don't have to. I guess it's like & VS &&.
* **Equational Reasoning** Sometimes mathematicians like proofs. What can I say?

#### 1.4 History!

Some interesting historical background is included. Again, I advice you: buy the book!

#### 1.5 Time For A Dabble!

<pre><code>sum [] = 0
-- notice this recursive example
sum (n:ns) = n + sum ns
-- illustrated by:
sum [42, 32, 64]
= ...
42 + sum [32, 64]
= ...
42 + (32 + sum [64])
= ...
42 + (32 + (64 + sum []))
= ...
42 + (32 + (64 + 0))
= ...
138
</code></pre>

*(Thank goodness I can still do basic arithmetic)*

* *Zero Is The Identity For Addition* (When I think of identity, if I think of: $f(x) = x$)
* $0 + x = x\ $  and $\ x + 0 = x\ |\ ∀\ x\ ∈ $ ... the set of ... Real numbers? Haskell has to deal with floating point values, so long as the bounds of 'Real Numbers' falls within the discrete domain of what is computable, then, I imagine (and I've never come across this before BTW, I would think of this as a **base case**, but I've never seen it expressed like this..? It just looks like a demonstration of the associative property) by numbers, the author means: real numbers. *Jeez, I need to brush up on my maths!*


...

Right, I am not too sure what is part of the UoE course and what isn't. I don't want to implement any answers in these rough notes that may be copied, so I'm going to leave it here for now... I'll catch up on the online lectures and I'm fairly certain (given there is a weekend coming up) I can get through the first half of this book in a couple of days.

**Yes, I am that boring...**

*Note: these ARE rough notes, they're not presented in the same fashion as the Plutus notes, these are mainly for me and my own learning. To be continued...*