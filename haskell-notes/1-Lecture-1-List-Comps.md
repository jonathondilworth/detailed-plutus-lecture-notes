# Lecture 1. Introduction (Computation & FP)

*Authors Note: It would seem appropriate to open this set of notes with a couple of quotes from the man himself (not to devalue any contributions made by others). I found [this video](https://youtu.be/iSmkqocn0oQ) particularly funny back when I was an undergraduate and it still makes me laugh now. Please, enjoy!*

>"Where we would like to go is here! This is Nirvana! Where we're both safe and useful!" <br />
— Simon Peyton Jones [[1]](#1)<br />

<div style="line-height:1.9"></div>

>"Us geeky Haskell guys started with a completely useless language... You have this black box and you press GO, and... it gets hot." <br />
— Simon Peyton Jones [[1]](#1)</div>

<hr />

### 1. I'm A Fool

Take my word for it.

### 2. Haskell: List Comprehensions

I can't quite remember if this was the 'nerdy thing' one of the guys from the aforementioned video was talking about (he may have been referring to to LINQ, which is a pseudo-SQL kind of shorthanded language for C# if I recall correctly). Please note: I've not written C# in years either! But, I do remember LINQ, because I thought: this is kind of weird...

Right, **let's get busy!**

### 2.1 List Comprehensions: Nomenclature

So, if you come from a pure computer science background with a focus on software development or applicational use rather than theoretical or research based approaches, you may find some interesting new vocabulary within the #haskell community!

Personally, I like it! Furthermore, whilst <code>x</code> or <code>xs</code> may sound like a highly unconventional naming convention for ***formal*** and/or ***actual*** parameters (or variables), I think perhaps you'll grow to enjoy using them!

I know what you're thinking... Yes, code should be easily readable (thanks for that, Uncle Bob[[2]](#2)). However, the problems associated with variable/parameter names as outlined within imperative OOP or imperative procedural languages are almost a consequence of the any given language design. Within declarative functional programming languages, it almost feels like you're writing mathematics and proofs, so you can be highly expressive (and capture large amounts of computational complexity) in a much shorter form. Just, wait and see...

**Nomenclature / Vocabulary / Conventions:**

* GHC — Glasgow Haskell Compiler 
* GHCi — See Above and add: tolower "Interactive" to the end!
* <code>[]</code> — An empty list.
* <code>|</code> — Given.
* <code><-</code> — Drawn from.
* <code>[1, 2, 3]</code> — A list of three elements.
* <code>,</code> — A guard.
* <code>sq :: Integer -> Integer</code> — Function (named sq) Declaration (somewhat of a signature) that accepts an Integer parameter and returns type Integer (later on I believe we'll learn about Monads, you can do cool stuff like: <code>sq :: Integer -> maybe Integer</code>, in this instance, the return type is... non-deterministic?
* <code>sq x = x * x</code> — Since the function <code>sq</code> has been declared, we can define the behaviour of the function by assigning (for lack of a better word) a procedure (or equation?) to it. It's important to note that the x following the function name <code>sq</code> is a **formal parameter** as appose to an **actual parameter**, which we'll come across later.
* <code>x <- [1, 2, 3]</code> — This **generates** a list. Thus, it is known as a **generator**, you'll see it in **list comprehensions** such as<br /><br /><code>SumSq :: Integer -> Integer</code><br /><code>SumSq n = sum [ sq x | x <- [1..n] ]</code><br /><br />It is also important to recognise that the <code>x</code> within the assignment (RHS) are **actual parameters**.
* <code>:{</code> — this is the beginning of a code block (if you require multiple lines to write within the GHCi, it's closed off using <code>:}</code>.

### 2.2 Right Then, Lists & Some Examples

What you may consider an array, or even a list in say python (although I can't remember if those guys call them vectors, why can't we just all agree - you know, **reach consensus**... let's not go off on a tangent about the Byzantine empire) is known as a list in Haskell. Easy right, wait until we get to monads and monoids! These mathematicians have some crazy naming conventions! Anyway, lists.

### 2.2.1 Easy Examples!

Let's get the obvious out of the way, we can both declare and define a list at the same time, but this isn't... *functional* (at least, I don't believe it is). Ideally, you want to know tend towards deterministic computations (programs) as much as possible. Anyway, see the below code:

<pre><code>> xs = [1, 2, 3, 4, 5] -- xs is convention for a list param
> :t xs -- view the type of the list we just declared & defined
> -- returns the following value
> -- which reads: xs is a list made up of an arbitrary number of 'Num' element data types
> -- Num values are considered as being within a 'typeclass' which is a group of types (numbers) 
xs :: Num a => [a]
</code></pre>

In short:

<pre><code>> xs = [1,2,3,4,5]
> :t xs
xs :: Num a => [a]
</code></pre>

### 2.2.2 Functions You Can Declare (and Define) To Modify Lists

Lists aren't much use unless you can do something with them! So, let's perform some operations on them. Firstly we need to define a function... Let's say: a square function. See below:

<pre><code>> squ :: Integer -> Integer -- remember: func dec
> squ x = x * x -- function def
> -- if we test this:
> squ 5 -- 5^2
25
</code></pre>

**Right, so what exactly are we doing?** 

Now let's do something useful (well, kind of)... Sum(sq, for all natural numbers between 1 and 5)...

<pre><code>> ssq :: Integer -> Integer -- I think we get this now :)
> ssq i = sum [squ x | x <- [1..i] ] 
> -- given param i, we create a list [1,2, ... , 4, 5], this is the generator
> -- each of these values are squared using our squ function
> -- they're then summed up and displayed
> -- this is read: summate the square of x given all x drawn from the generator [1..5]
> ssq 5
55
</code></pre>

### 2.3 Clarification

Let's get some code down and comment it to explain what each part is responsible for...

**Input:**

1. <code>-- double hyphen is a comment!</code><br />
2. <code>y :: Integer -> [Integer]	-- function declaration</code><br />
3. <code>y n = [x | x <- [1..n]] 	-- function definition</code>

<small>*Note: I'm assuming you're coming at this from an imperative OOP / procedural programming background.*</small>

* The first line is self-explanatory, comments. Great!
* The second line is a **function declaration** [[3]](#3), essentially a 'function signature', the declaration specifies the name of any given function to later be defined. Further, it provides the list of arguments (or parameters) as separated by the <code>-></code> symbol. The final specified item in the declaration is the return type. In this instance we are declaring a function named y, which takes a single Integer as a parameter and returns a list of Integers as its return type. Now, we need to define the function...
* The third line is the function definition. This defines how the declarative function operates. The first character is, again, the function name. The second character <code>n</code> is a formal parameter and is representative of Integer found within the function declaration. The **LHS** is essentially saying: <code>f(n) = ... -- some operation that involves n</code>. The **RHS** is known as a list comprehension. It is read as: ***return a list of elements such that for all 'x' drawn from a count of 1 to n is contained within the list.***

**Calling The Function: <code>y 5</code>**

As the function name is <code>y</code> and, as explained above the function takes one parameter. Thus, in order to call the function we provide the function name <code>y</code> and (typically) all the **formal parameters** that the function is excepting to accept, given then declaration.

In this case we simply call: <code>y 5</code>

**The Return Value**

<code>[1, 2, 3, 4, 5]</code>

**Some Example Code For You To Gloss Over :)**

<pre><code>Prelude> :{
Prelude| sq :: Integer -> Integer
Prelude| sq x = x * x
Prelude| :}
Prelude> :t sq
sq :: Integer -> Integer
Prelude> sq 5
25
Prelude> :{
Prelude| facto :: Integer -> Integer
Prelude| facto f = sum [sq x | x <- [1..f]]
Prelude| :}
Prelude> :t facto
facto :: Integer -> Integer
Prelude> facto 5
55
Prelude> -- opps! not factorial!
Prelude> :{
Prelude| factoria :: Integer -> Integer
Prelude| factoria f = product [ x | x <- [1..f] ]
Prelude| :}
Prelude> :t factoria
factoria :: Integer -> Integer
Prelude> factoria 5
120
Prelude> -- Huurah!
</code></pre>

**What are your thoughts? Reflect on the information contained within this document, discuss it with your friends and make sure you understand what is going on! :) Good Luck!**

### References

<a href="#1" id="1">1</a>. <https://youtu.be/iSmkqocn0oQ>

<a href="#2" id="2">2</a>. Martin, R.C., 2009. <br />
Clean code: a handbook of agile software craftsmanship. <br />
Pearson Education.

<a href"#3" id="3">3</a>. <https://www.tutorialspoint.com/haskell/haskell_functions.htm>

