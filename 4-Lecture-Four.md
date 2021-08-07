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
	* Effects Systems
	* Streaming
	* Monads
	* Contract Monads

### 2. An 'Advanced' Haskell Primer

*... see below?*

Summary:

* Monads = Burritos (Ask Lars).
* Monads are usually the first stumbling block for people who are not use to Haskell.
* Brief introduction to Monads.
* Before we get to Haskell, let's look at Java (Unsafe and Useful!)
* Haskell: IO Monad.
* 

### 2.1 Imagine: Mainstream A "Unsafe" "Useful" Language: Java

Firstly, when it comes to imperative programming<sup><a href="#fn1">1</a></sup> a single function may do one of many things<sup><a href="#fn2">2</a></sup>. In the world of 'Smart Contracts' which may be dealing with peoples' livelihoods and quite importantly, their money, it's a good idea to try and limit the amount of 'things' such a contract is able to do. Essentially, go and watch the Simon Peyton Jones video about safe and unsafe programming languages [[2]](2), it is somewhat tongue in cheek (at least, the title was at the time), but there is **a lot** of sense to it. Please see below for an example of an "unsafe" "useful" programming language.

**Java Example:**

<pre><code>import java.lang.Integer;

Public Static I makeSomethingHappen() {
	// ...
}
...
System.out.println(makeSomethingHappen();
...
makeSomethingHappen();
</code></pre>

Even though the above code will always return a value of type int, since we do not know what the ... executes (if it was not a comment, of course!), it may not always return the same <code>int</code> value. This is due to the fact that the Java program could be performing arbitrary input/output, it may depend on any number of, say: global variables, connections to a DB, API calls, etc. Essentially: it's unpredictable in nature. It's unsafe. But, it can be pretty useful (Thanks Simon!).

### 2.1.1 A More Concrete Unsafe (But Rather Non-Useful) Example

To demonstrate what we're talking about here more concretely, I wiped out my old Java book from about ten years ago and threw together the following code. Now, prepare to be disgusted, because Java programmers use notoriously long variable names. You could even say: the polar opposite of Haskell programmers (as most of them are mathematicians, so <code>x</code> seems to be perfectly adequate to provide a useful naming convention for mathematicians)! Anyway, see below *(and yes, Java is unnecessarily verbose)*.

<pre><code>import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.StringBuilder;
import java.net.MalformedURLException;
import java.io.IOException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

class Unsafe
{
	/**
	 * Get the HTTP repsonse body (in string format) at a spec'd URL
	 * 
	 * Usually this would be refactored, but this is a simple example
	 * In addition, there are ample comments here, so.. do one.
	 * 
	 * @param fromUrl A string: HTTP endpoint
	 * @return        The contents at the fromUrl param
	 */
	public static String getSingleLineHttpResponse(String fromUrl)
	{
		// typical Java; x aint good enough we have to use long var names
		// the code should read such that it doesn't need comments
		// ref: Uncle Bob: Clean Code
		try
		{
			URL webAddress = new URL(fromUrl);
			HttpURLConnection webConnection = (HttpURLConnection) webAddress.openConnection();
			if(webConnection.getResponseCode() != 200)
			{
				throw new IOException("Error: Sorry, but the connection was unsuccessful.");
			}
		    BufferedReader someMemReader = new BufferedReader(new InputStreamReader(webConnection.getInputStream()));
		    return someMemReader.readLine();
		}
		catch(MalformedURLException malformedException)
		{
			return "Error: the supplied URL is malformed or.. BAD";
		}
		catch(IOException inputOutputException)
		{
			return "Error: the input resource is unavailable or.. BAD";
		}
	}

	/**
	 * Simply removes an expected value from square brackets
	 * ANY Exception returns -1
	 * 
	 * @param singularJSONInput	a singular line of JSON [...]
	 * @return a value between 1 and 100 or -1 on error
	 */
	public static int extractIntFromSquareBrackets(String singularJSONInput)
	{
		try
		{
			Pattern valuesWithinAnySquareBrackets = Pattern.compile("\\[(.*?)\\]");
			Matcher matches = valuesWithinAnySquareBrackets.matcher(singularJSONInput);
			String value = matches.find() ? matches.group(1) : "-1";
			return  Integer.parseInt(value);
		}
		catch(Exception e)
		{
			return -1;
		}
	}

	/**
	 * Gimmie' a 5! (of type int)
	 * 
	 * @return 		(int) 5
	 */
	public static int doSomething() 
	{
		return 5;
	}

	/**
	 * ...
	 * 
	 * @return If a random value between (1 : 100 + 5) > 54 then True else False
	 **/
	public static boolean doSomethingComplex()
	{
		String randomFromAPI = getSingleLineHttpResponse("http://www.randomnumberapi.com/api/v1.0/randomnumber");
		int randomNumberFromAPI = extractIntFromSquareBrackets(randomFromAPI);
		return ((randomNumberFromAPI + doSomething()) > 54);
	}

	/**
	 * The imperative, unsafe (but useful) program!
	 */ 
	public static void main(String[] args)
	{
		// Let the box heat up... can we know the result? NOPE.
		System.out.println(doSomethingComplex());
	}
}
</code></pre>

We can observe the (somewhat predictable) unpredictable output of this code (yes, it does compile).

![./img/unsafe.jpg](./img/unsafe.jpg)

><em>COFFEE TIME! You may remember me writing something about including 'coffee time' within some of my lecture notes, they're essentially exercises that I have thought about, that I might want YOU, the reader (if there are any) to think about.
>
> Question: Right, so we can all make out that this code will TYPICALLY return True or False, however, are there any other possible outcomes? What are they? Why might they occur?</em>

The point is: **arbitrary input output from any data source is possible anywhere within the programme, during execution**. The output coming from these sources may (and is very likely to) change, making it difficult to test. The further away we get from deterministic output (or, simply a small set of possible predictable outputs), the harder it becomes to test. When we're playing with money, **this is not a good thing!**

### 2.2 Now: For Some Haskell

So, you like the sound of declarative programming do you? **Good for you!** Personally, I am indifferent. I enjoy playing Snake on a Nokia 3310, but at the end of the day, you select the correct tool for the job. In my personal opinion (and I believe I can speak for the majority of the Cardano community here when I say): some kind of functional programming is the most appropriate tool when it comes to the implementation of smart contracts. Haskell is, as you well know, a functional programming language, so let's see an example.

**Haskell Example:**

<pre><code>foo :: int
foo = ...
	
let x = foo in ... x ... x ...
... foo ... foo
</code></pre>

*Note: Haskell is a pure functional language*

*Now I'm going to assume you also are clued up on your basic philosophy:*

Premise 1: Given that the value of calling <code>foo</code> is unknown, and <br />
Premise 2: Assuming foo has been assigned a return value<sup><a href="#fn4">4</a></sup> <br />
Conclusion: Then foo will **always**<sup><a href="#fn5">5</a></sup> return the same value [[1]](1) <br />

**Haskell knows that you're not a liar, so it keeps you to your word! This is explained in §2.3 as referential transparency.**

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

<a href="#1" id="1">[1]</a> Lipovaca, M., 2011. Learn you a haskell for great good!: a beginner's guide. no starch press.

<a href="#2" id="2">[2]</a> Haskell is useless. Simon Peyton Jones. December 18, 2011. <https://youtu.be/iSmkqocn0oQ>

### Footnotes

<a href="#fn1" id="fn1">1.</a> Which, for most of us, we know all too well... For those who maybe are stuck in the ancient past and are still using punch cards: these are languages which essentially say: do this, now do this, now do this (sounds more like procedural, but, not to go down a rabbit hole here, procedural programming is simply a subset of imperative programming); Procedural is typically thought of as similar to assembly (do this, now do this, now go here, now do that), where as object-orientated (still imperative) has a more elegant (and possible eloquent) way of describing, defining and maintaining state. However, we do also have declarative programming languages, functional programming falls into this camp, and thus, Haskell is a declarative programming language.

<a href="#fn2" id="fn2">2.</a> Making them fairly difficult to test to the same level of scrutiny as say something such as a mathematical function<sup><a href="fn3">3</a></sup>. Since Haskell is a functional programming language, this makes Haskell pretty darn easy to test. Thus, pretty darn safe (once again, thank you Simon [[2]](2)).

<a href="#fn3" id="fn3">3.</a> To my limited mathematical knowledge: A mathematical function is defined (almost as though it is some kind of constant) as having an input and facilitating an output. A 'fruity' question I had to ask myself was: how exactly can you implement a mathematical, functional programming language within a discrete system? The obvious answer being: discrete mathematics... However, now we have to apply a **whole bunch** of constraints to such a language, which is probably why it's so safe? At least, perhaps one reason why? These are my notes, so take them for what they're worth, which may be absolutely nothing.