# Lecture Zero?

*A Quick Note:*

*It doesn't matter if you've never done* **ANY PROGRAMMING BEFORE** *or if you've done* **LOADS OF PROGRAMMING BEFORE** *- if you're reading this, you've likely not done much functional programming, so we're all on an even keel! So, please:* **Do Not Worry!**

### 1. Introduction

### 2. Language

Language is often taken for granted. Two main methods to communicate:

* Prose (defined by Google as: "written or spoken language in its ordinary form, without metrical structure")
* Mathematics ("the abstract science of number, quantity, and space, either as abstract concepts or as applied to other disciplines")

> "This is where I think language came from ... **But when it gets really interesting I think is when we use that same system of symbols to communicate all the abstract and intangible things ...**"

##### A New Type Of Language

* Computing: New Way Of Expressing Ideas

*May be worth expanding upon this point, computation as a new form of expression, I'm not the sharpest tool in the toolkit, I didn't quite follow. Just an observation.*

> "A language that doesn't affect the way you think about programming, is not worth knowing" <br /> 
> — Alan Perlis, 1922-1990

For those interested in Philosophy of Language, I would encourage you to look into some of the following authors:

* Wittgenstein
* Chomsky
* Searle (if you're doing informatics, you need to read John Searle)

*FYI: John Searle's arguments have progressed since the Chinese room.*

### 3. Functional Programming - Why Does It Matter?

*In my humble opinion, all technical domains matter (somewhat tongue-in-cheek).*

**But, for the sake of these lectures, Functional Programming Matters Most!**

#### Some Functional Languages:

> "Safe, but useless!"

* Elm
* Erlang
* F#
* Haskell
* Javascript
* Scala

#### Some Imperative OOP (Most Widely Used):

> "Here are unsafe but useful!"

* C++
* F#
* Java
* Javascript
* Python
* Ruby
* Scala

### 4. Additional Notes

You can write most languages in a functional style, which is why you're seeing an intersection between functional languages and imperative OOP languages. Scala is a great example.

FP is great for concurrency. You can split elements of large computations amongst many different 'threads', 'cores', 'machines'...

FP -> Safe (in general, safer than other languages that allow for arbitrary IO through every orifice); thus, good for finance.

Who teaches FP as introductory courses for Computer Science / Informatics?

*Not bloody Manchester, I'll tell you that now! I had a shock when I got to Edinburgh.*

* Oxford
* Cambridge
* Imperial
* Edinburgh

*At some point in this course we're going to look at Map-Reduce (which Google implements as.. Hadoop? Or do I have that backwards?)*

**Additional Topics Covered:**

* Garbage Collection (Pretty sure we've been doing this for a while...)
* High Order Functions
* Generics (We see these in Java already)
* Type Classes
* List Comprehensions (LINQ in C#, Generators in Python?)

### 5. Functions

> "Functions are like recipes!"

**Example:**

<code>f(x) = x^2</code>

= Parabola 

**Types:**

* Integer: whole number
* Float: e.g. 32-bit floating point (8-bit exponent, 23-bit mantissa)
* Char: ['s' : ['i','n','g','l','e']] ++ ["Quotes"]
* String: "Double Quotes"
* Bool: True, False <code>:t True</code> ... <code>True :: Bool</code>
* Pictures (?)

*This is the part where you work through the tutorial by yourself.*

### 5.1. Function Composition

**The Way In Which Functions Are Read In Haskell:**

> An extract from "Thinking Functionally With Haskell: <br /><br />
> "In mathematics one can encounter expressions like $logsinx$. To the mathematician that means $log(sinx)$, since the alternative $(log sin) x$ doesn't make sense. But in Haskell one has to say what one means, and one has to write $log\ (sin\ x)$ because $log\ sin\ x$ is read by Haskell as $(log\ sin)\ x$."

**Thus, we declare functions:**

*(If using GHCi, import Data.Int)*

<code>sq :: Int -> Int</code>

And assign functions:

<code>f x = x * x</code>

And call functions:

<code>f 10</code>

Which results in:

<code>100</code>

![./img/function-example.png](./img/function-example.png)

> "The order of composition is from right to left because we write functions to the left of the arguments to which they are applied."

Next Lecture.