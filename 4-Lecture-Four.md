# Lecture Four

### 1. Introduction

*Currently Being Written... In Progress... I'm doing my best!*

* On-Chain
	* Validation Logic
	* Compiled to Plutus Script
	* Lives 'on-chain'
	* Executed by Network Nodes
	* Not yet looked at complex examples
	* Context
	* Inputs and Outputs
	* Native Tokens (Minting and Burning)
* Off-Chain
	* Let's not neglect the off-chain part
	* In order to get on-chain validation to... validate, we need to build Tx and submit to on-chain UTxOs
	* Don't have to worry about compiling...
	* Writing plain old Haskell
	* This = We use sophisticated Haskell
	* We use a special Monad, called the Contract Monad
	* Thus we begin with an 'advanced' Haskell Primer

### 2. An 'Advanced' Haskell Primer

*... see below?*

### 2.1 Imagine: Mainstream "Unsafe" "Useful" Language: Java

**Java Example:**

	Public Static int makeSomethingHappen() {
		// ...
	}
	...
	makeSomethingHappen();
	...
	makeSomethingHappen();
	
Even though the above code will always return a value of type int, since we do not know what the ... executes (if it was not a comment, of course!), it may not always return the same <code>int</code> value. This is due to the fact that the Java program could be performing arbitrary input/output, it may depend on any number of, say: global variables, connections to a DB, API calls, etc. Essentially: it's unpredictable in nature. It's unsafe. But, it can be pretty useful (Thanks Simon!).

### 2.2 Now: For Some Haskell

**Haskell Example:**

	foo :: int
	foo = ...
	
	let x = foo in ... x ... x ...
	... foo ... foo
	
Regardless of what is executed during the ... we know that the output from the Haskell script is always going to be the same. Once you declare a function or a value, Haskell knows that you're not a liar, so it keeps you to your word! This is explained in §2.3 as referential transparency.

### 2.3 Referential Transparency

The following is an extract from a very well known Haskell tutorial / book:

> In purely functional programming you don't tell the computer what to do as such but rather you tell it what stuff is. The factorial of a number is the product of all the numbers from 1 to that number, the sum of a list of numbers is the first number plus the sum of all the other numbers, and so on. You express that in the form of functions. You also can't set a variable to something and then set it to something else later. If you say that a is 5, you can't say it's something else later because you just said it was 5. What are you, some kind of liar? So in purely functional languages, a function has no side-effects. The only thing a function can do is calculate something and return it as a result. At first, this seems kind of limiting but it actually has some very nice consequences: if a function is called twice with the same parameters, it's guaranteed to return the same result. That's called referential transparency and not only does it allow the compiler to reason about the program's behavior, but it also allows you to easily deduce (and even prove) that a function is correct and then build more complex functions by gluing simple functions together. [[1]](#1)

### 2.4 The IO Type Constructor

IO is a type constructor, such that you can define something such as:

	foobar :: IO Int
	foobar = ...
	
The IO type constructor is described as by Lars as a 'recipe' to produce an Int. He insists that it does not break referential transparency, so I can only assume that by recipe he essentially means: some code exists, which may be assigned to a variable of type (constructor) IO Int. The code itself never changes, in fact, nothing can possibly change, the only thing that can change is the actual Input (or/and Output, depending on what the function that uses IO Int does). It is, however, important to note that the function cannot do much insofar as, it cannot actually do anything except accept input, perform a function and then, possibly display output; and THUS: referential transparency is maintained. I may need to do some more reading...

*But first... Let it be known that I wrote about 21 lines of Haskell in my entire career before this course, so please don't judge me too harshly if I get something wrong! These notes are here for my own sake and if they are helpful for others, that's great too!*

*May need to revisit this after I've slept...*

### References

<a href="#1" id="1">1.</a> Lipovaca, M., 2011. Learn you a haskell for great good!: a beginner's guide. no starch press.

