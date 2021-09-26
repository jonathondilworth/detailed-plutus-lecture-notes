# Lecture Zero?

*A Quick Note:*

*It doesn't matter if you've never done* **ANY PROGRAMMING BEFORE** *or if you've done* **LOADS OF PROGRAMMING BEFORE** *- if you're reading this, you've likely not done much functional programming, so we're all on an even keel! So, please:* **Do Not Worry!**

### 1. Introduction

### 2. Language

Language is often taken for granted. If one is to truly reflect on the subject of language, you could say we live in a world that it constructs. There exist a lot of interesting ideas within the Philosophy of Language *(which we'll get to in a minute)*, but for now let's focus our attention on communication. Communication is a form of expression, up until recently we've only *really* had a couple of methods to communicate:

* Prose (defined by Google as: "written or spoken language in its ordinary form, without metrical structure")
* Mathematics ("the abstract science of number, quantity, and space, either as abstract concepts or as applied to other disciplines")

> "This is where I think language came from ... **But when it gets really interesting I think is when we use that same system of symbols to communicate all the abstract and intangible things ...**"

##### A New Type Of Language

* Computing: New Way Of Expressing Ideas

*Note From Author: I don't particularly follow here... I mean, yes, computing involves new languages, but are we not just constructing domain specific lexicons, various types of syntax and building software (assemblers, compilers, interpreters) to reduce this 'new way of expressing ideas' back into, well, essentially mathematics? Boolean logic? I suppose we can create various representations, including novel visual representations. Nevertheless, it's difficult to draw a connection between computing and a novel way of expressing ideas. Then again, I suppose BitCoin gave birth to UTxO and IOHK / IOG extended that model. You could call that an evolution of financial models, which I suppose is a new way of expressing ideas. I feel as though this section needs a little bit of clarification. How exactly is computation so novel if we have had devices such as "Napier's bones" which date back to, the 1600s?*

> "A language that doesn't affect the way you think about programming, is not worth knowing" <br /> 
> - Alan Perlis, 1922-1990

For those interested in Philosophy of Language, I would encourage you to look into some of the following authors:

* Wittgenstein
* Chomsky
* Searle (if you're doing informatics, you need to read John Searle)

*FYI: John Searle's arguments have progressed since the Chinese room.*

### 3. Functional Programming - Why Does It Matter?

Right, here's my opinion: given a task or a computing problem, a competent software engineer should be able to select the most appropriate set of tools for the job. Thus, one should be experienced in transitioning between different paradigms (declarative, imperative, purely functional, OOP, modular, scripts, etc). As such, experience in, well, to be honest: as many technical domains as possible is preferable. But, unless you have all the time in the world, well - you have to specialise somewhere. Anyway, both declarative (purely functional, to somewhat functional and formal logic) and imperative languages (modular, procedural and object orientated) need to be explored during ones adventure through the otherworldly realm of computing!

*In my humble opinion, all technical domains matter.*

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

Remember: Functional Programming Languages are pretty safe (as IO is minimised and you steer more towards small deterministic programs that can be easily tested and put together to construct something larger, which, hopefully has an effect on the world... Well, safety tends to increase. In short: less unexpected IO => safer). Thus, FP is pretty good for finance.

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

You all know what a parabola looks like, right? If not, Google is your friend!

[https://www.google.com/search?q=f(x)+%3D+x%5E2&oq=f(x)+%3D+x%5E2](https://www.google.com/search?q=f(x)+%3D+x%5E2&oq=f(x)+%3D+x%5E2)

**Types:**

* Integer: whole number
* Float: e.g. 32-bit floating point (8-bit exponent, 23-bit mantissa)
* Char: ['s' : ['i','n','g','l','e']] ++ ["Quotes"]
* String: "Double Quotes"
* Bool: True, False <code>:t True</code> ... <code>True :: Bool</code>
* Pictures (?)

*This is the part where you work through the tutorial by yourself.*
