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

*How many people feel that they understand recursive data types? Paused video.*

As far as I understand, recursive data types (so long as we're talking data structures here) are sturctures that can be parsed recursively. So, an obvious example would be a tree, as you can recursively search a tree using a BFS or a DPS, right, or wrong? I guess we'll see.

*Unpaused video*

Expression Trees are an example of a recursive data type. Right, so I guess trees sprung to mind because of the lecture title, but I didn't even realise that until after the fact! Sounds like I'm on the right path.

*Paused video AGAIN, at nine minutes!*

*You know, I was initially confused - what is lit? etc. I was just being stupid, lit is literal, duh. I've paused the video at 9 minutes and I'm fairly certain that these expressions are essentially being built as trees, such that they can then be evaluated, given some constraint(s).*

*Somewhat off-topic: An example of this that instantly sprung to mind was Abstract Syntax Trees within compilers. If you represent the source input as an Abstract Syntax Tree (after lexing / tokenisation), you can recursively parse it, then annotate it and finally translate it to an intermediary representation, or into its target language.*

*Unpaused video*

I am fairly certain I'm correct. However, I believe there is some required reading that I may have skipped as I've just been watching the lectures. Thus, I'm going to come back to this after checking up on any pre-requisite reading and actually doing any assigned reading.