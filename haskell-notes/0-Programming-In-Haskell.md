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

*Note: these ARE rough notes, they're not presented in the same fashion as the Plutus notes, these are mainly for me and my own learning. To be continued...*

<hr />

*So.. To recap:*

If we define a recursive sum function as follows:

<pre><code>summ :: Num a => [a] -> a
summ [] = 0
summ (x:xs) = x + sum xs
</code></pre>

We're telling Haskell that our function <code>summ</code> takes a 'Generic Number' type in the function signature (so, really it's quite flexible in terms of what we can provide as a number, similarly if we were to say that it was a type that could be ordered: Ord a, then any Type Class that is order-able is accepted as a parameter, so long as the condition is satisfied such that each parameter a is of that class. Otherwise the compiler will get angry).

Then, we're assigning an initial value of zero to the to summ in the instance where an empty list is passed to the function.

Further, when a list that contains values (of Type Num) is passed to summ, the function is going to take each 'first' element <code>x</code> in the list (since the cons of each first element equates to the full list) for each value of the list <code>xs</code> ... <code>(x:xs)</code>, and it's going to <code>+</code> (add) until the empty list is reached (and the summation of all elements has been computed).

It's really kind of a strange way of thinking about how these operations are carried out. **It's easy enough to read, it's easy enough to understand, but it's kind of difficult to come up with this stuff on your own.** It is as though you have to change the way you've been looking at problems this whole time. It's weirdly horribly elegant.

**Similar, let's say we want to take the product of every element within a list:**

<pre><code>pd :: Num a => [a] -> a
pd [] = 1
pd (x:xs) = x * pd xs 
</code></pre>

Let's work through this one:

<pre><code>pd [5,7,1,2,4]
-- apply pd
5 * pd [7,1,2,4]
-- again
5 * ( 7 * pd [1,2,4])
-- again
5 * ( 7 * (1 * pd [2,4]))
-- again
5 * ( 7 * (1 * ( 2 * pd [4])))
-- again
5 * ( 7 * (1 * ( 2 * ( 4 pd []))))
-- again, remember pd [] = 1
5 * ( 7 * (1 * ( 2 * ( 4 * 1))))
-- the answer!
280
</code></pre>

**An Implementation of QuickSort, Reverse QuickSort and Chapter Exercises Can Be Found In Exercises.**

### Chapter 2. First Steps

* some notes on GHC and GHCi
* some notes on installation
* The Prelude (no, not Bach) - It's called the Standard Prelude, essentially a bunch of functions that are bundled with GHC.
* **head** [1,2,3] returns 1
* **tail** [1,2,3] returns [2,3]
* [1,2,3] **!!** 2 (zero-based-index) returns 3
* **take** 2 [1,2,3] returns [1,2]
* **drop** 1 [2,1,3] returns [1,3]
* **length** [1,2,3,4] returns 4
* **sum** [1,2,3,4,5] returns 15
* **product** [1,2,3,4,5] returns 120
* [3,2,1] **++** [6,5,4] returns [3,2,1,6,5,4]
* **reverse** [5,4,3,2,1] returns [1,2,3,4,5]

**Some Important Stuff!**

Some Familiarity... Mathematics, function notation:

$f(a,b) + cd$ means add the result of c $\times$ d to the value of $f(a,b)$, whatever that happens to be.

* Multiplication of two values is often denoted silently... $cd$
* The application of a function to its arguments is usually denoted by enclosing the arguments in parentheses: $f(a,b)$
* In Haskell things are a little, different. The application of arguments to functions is denoted silently using whitespace, and multiplication is explicit: <code>f a b + c*d</code>
* NOTE: Function Application has higher priority than all other operators!
* <code>f a b c + d === (f a b c) + d</code>
* <code>f(g(x)) === f (g x)</code>
* <code>f(x, g(y)) === f x (g y)</code>
* <code>f(x)g(y) === (f x) * (g y)</code>

*(Includes some information on Haskell scripts and GHCi)*

* You can write division as a function: <code>div (f x) (g x)</code>
* It is, however, more natural to write: <code>f x `div` g x</code>
* Reloading scripts in GHCi: <code>:reload</code>

**The Layout Rule**

Each definition at the same level must begin in precisely the same column. Like indentation in Python, only by convention in Haskell you use the same column and don't simply indent by 2 spaces, for example.

**DON'T USE TABS.**

**Nested Comments: {- -}**

*(Types and Classes Up Next)*

<hr />

### Types and Classes

* Type Inference: Types are recognised at compile time. Haskell does a great job of inferring types. For example, you can have fairly loose type-classes, like Num? Where you can either be working with an Integer, float, double, whatever.

*Every expression must have a type. Type Inference is evaluating any given expression (without explicit types) at compile time, if you've done something wrong, you'll get a type error. e.g. True == 3. Where as other languages are not as fussy. For example, in PHP the exact same code will return 1, which also is considered to be True. In fact, any value that is not NULL or False is True (in PHP), unless you specify an equivalence operator: 3 === True.*

**When writing our functions, we can choose to give them an explicit type declaration.** This is considered to be 'good' when you're building robust code.

* What exactly is a Type? It can be thought of as a collection (perhaps a set) of values. For instance: Bool is a Type which can take on the value True or False.
* Every expression must have a type. At compile time, in order to evaluate an expression, each operand must belong to the same type-class, such that the set of operations defined by the expression is computable.

* **Haskell Programs are TYPE SAFE** because type inference precedes evaluation.

* Bool := {TRUE || FALSE }
* Char: individual characters separated by single quotes 'a', 'b'
* String (x:xs) = x : String xs | base case: [] 
* Int (fixed-precision) an Integer with an assigned 'space' in memory (2^63 - 1 .. -2^63) <- prone to overflow if values are too large (fall outside the specified memory).
* Integer (arbitrary-precision) better performance typically, also avoid the potential of unforeseen results.
* Float (single-precision floating point numbers), precision depends on the magnitude of the number, since a floating point value (in memory) is required to store (32 bits, or 64 bits, I'm going to go ahead and say these are 64 bit floating point values) a signed bit, 11 bits for the exponent and the remaining 52 bits for the Mantissa.
* Double x2 precision floating point number.

#### Other Types (and Function Signatures)

* Singleton Lists: ['a'], [1], AND [[]].
* Apparently you can get an infinite list (discrete system?)
* Curried Functions (not so obvious):
	* consider add' :: Int -> (Int -> Int)
	* add' x y = x + y
	* the return type can be fed as the parameters into another function that takes (Int -> Int)
* Polymorphic Types
	* length as an example, provides the length of a list irrespective of the element type within the list.
	* We typically denote this kind of declaration using lowercase a, b, c, etc: length :: [a] -> Int... Examples below:

```haskell
fst :: (a,b) -> a
head :: [a] -> a
take :: Int -> [a] -> [a]
zip :: [a] -> [b] -> [(a,b)]
id :: a -> a
```

**OVERLOADED TYPES**

Let's say we want a type that represents either an Integer OR a floating point value (Float or Double). As both Integer and Float fall into types that can be used by arithmetic operators, it would be nice if we could declare functions with these type of *class constraints*. Well, we can.

```haskell
(*) :: Num a => a -> a -> a
``` 

**Num** for *Numbers* is an overloaded class constraint.

#### Other Interesting Things

* Ordered Types
* Showable Types
* Readable Types
* Numeric Types (which we just went over)
* Integral Types
* Factorial Types

### Exercises

1. What are the types of the following values?

```haskell
['a', 'b', 'c'] -- a :: [Char]
('a','b','c') -- a :: (Char, Char, Char)
[(False,'0'),(True,'1')] -- a :: [(Bool, Char)]
([False,True],['0','1']) -- a :: ([Bool], [Char])
[tail, init, reverse] -- a :: [[a] -> [a]]
```



