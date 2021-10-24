### Lecture Nine

*A quick off-topic note (and fun fact!): since Eratosthenes comes up in an initial question during this lecture, I thought it may be worth pointing out one specific fact to any 'flat-earthers' that may be enjoying these notes! Eratosthenes also did calculate the circumference of the earth proving that, yes, it is in fact spherical.*

**Okay, let's get busy!**

How can we calculate prime numbers without using a library function? Here's my implementation:

```haskell
Prelude> isNPrime n = (length [x | x <- [2..n-1], n `mod` x == 0]) == 0
Prelude> primes = [3, 5, 17, 19, 83, 89]
Prelude> nonPrimes = [12, 14, 35, 49]
Prelude> map isNPrime primes
[True,True,True,True,True,True]
Prelude> map isNPrime nonPrimes
[False,False,False,False]
Prelude> -- success?
```

*We'll implement Eratosthenes' method later!*

This seemed to capture the essence of Haskell quite nicely. Thus, I thought it was worth quoting.

> "Uniformity and simplicity are a virtue!" <br />
> — Prof Philip Wadler, University of Edinburgh

**Data Types**

A <code>data</code> type appears to be defining some kind of structure.

*How many people feel that they understand recursive data types? Paused video.*

As far as I understand, recursive data types are essentially value containing values (where the cardinality of the set of values is arbitrary, thus the instantiation of such a type will result in a recursively parseable data structure). So, an obvious example would be a tree, as you can recursively search a tree using a BFS or a DPS, right? I guess we'll see.

*Unpaused video*

Expression Trees are an example of a recursive data type. A tree is in fact a data structure, so, great.

*Somewhat off-topic: An example of this that instantly sprung to mind was Abstract Syntax Trees within compilers. If you represent the source input as an Abstract Syntax Tree (after lexing / tokenisation), you can recursively parse it, then annotate it and finally translate it to an intermediary representation, or into its target language.*

```haskell
data Exp   =  Lit Int
           |  Add Exp Exp
           |  Mul Exp Exp
           deriving (Eq, Show)
```

I think it is important to take a moment and examine what exactly we're expressing (no pun intended) here with the above code.

An Exp (expression) as we're defining it (as a data type) can be a literal Int, the addition of two expressions (notice, an expression is the type itself, thus this data type is recursive), or the multiplication of two expressions. Furthermore, as as an expression can be (sigh) expressed in terms of itself, you may have an arbitrary number of expressions within an expression. Again, this can be then parsed and evaluated recursively.

For symbols which are constructors, the first 'letter' is always a <code>:</code>, in addition you also have to close the constructor with another <code>:</code>.

Outlines some information regarding order of precedence.

Some notes on Symbol infix notation.

It's kind of strange how my immediate thoughts in this lecture were associated with abstract syntax trees and I'm coming to learn that the entire point of recursive data types is to define your own a set of rules for a given type. As such, you are essentially building your own small programming language within Haskell. This allows us to be extremely expressive. Right, so functional languages are DSLs for DSLs (this is almost like taking a trip down the rabbit hole, recursively!).