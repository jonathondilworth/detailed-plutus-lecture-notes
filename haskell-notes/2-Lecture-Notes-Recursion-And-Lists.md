# FP Lecture Notes 2

*Topic: Lists & Recursion*

### 1. Introduction

* Remember to keep the following in mind: lists with one item in them are not the same as the item itself:

<pre><code>Prelude> 5 == [5] -- Error!</code></pre>


* Haskell guys don't particularly like loosely typed languages, they can become complex, difficult to model (computationally) and when you introduce unpredictable IO, you get a mess! Ideally programs should be as *pure* as possible (and ideally deterministic).
* Keep It Simple Stupid!
* Construct (Con): <code>(:) :: a -> [a] -> [a]</code>
* Append (Append): <code>(++) :: [a] -> [a] -> [a]</code>
* Remember Single Quotes & Double Quotes: <code>[Char] ['a','b','c'] == "abc"</code>
* Type Variables 
* Shadowing: two variables can have the same name, but the most recent value assignment is used during execution.
* 

### 2. More On Lists

Technically a list is a data structure (albeit, a fairly basic one), as such, you can perform operations on them. We're going to investigate two operators within this section: ```Construct``` and ```Append```.

### 2.1 Construct (:)

When it comes to lists in Haskell it is important to distinguish between a single item and a list with a single item within it. This may sound strange to those who are new to programming, but a single element is completely different a list that contains a single element. Think about it, you can perform all kinds of operations on lists. Here are some examples:

**Construct (Con):**

*(Using GHCi)*

<code>> 4 : [2] -- this will insert 4 at the beginning of the list</code>

Resulting in:

<code>[4, 2]</code>

However, be mindful that **Construct** is declared using the following function declaration (or, if you prefer, function signature):

<code>(:) :: a -> [a] -> [a]</code>

As a result, we cannot, for instance, use the Construct operator to combine two lists.

<code>> [4] : [2] -- Type Error!</code>

The first parameter should be an element with the same data type as every other element within the list. Consider a list of lists, you could use the Construct operator to insert a list into the list of lists. Why? Because first parameter is of the same data type as each of the elements in the list. *It may be worth noting that these lists can be of any length.* See the examples below:

<code>> [4] : [[2]]</code>

Resulting in:

<code>[[4], [2]]</code>

How about:

<code>> [4, 2] : [[4, 4], [2, 2], [4, 4, 4], [2, 2, 2]]</code>

This is perfectly valid, because each element is of the same data type, as is specified as a requirement within the function declaration. If you really want to see the result (it's absolutely mind blowing! Really, it's not):

<code>[[4,2],[4,4],[2,2],[4,4,4],[2,2,2]]</code>

Right, so you get it? **Great!**

### 2.1.1 Construct: Characters and Strings

Something that should be made **absolutely clear from the beginning of the adventure** is that a String is considered to be a list of Characters, well a list of <code>Char</code>, expressed as <code>[Char]</code>. This can be demonstrated by comparing a string (**always** expressed using double quotes) and a list of characters (each character within the list is **always** expressed using single quotes, this goes for individual characters outside of lists too).

*(Using GHCi)*

<code>"abc" == ['a', 'b', 'c']</code>

This returns a value of: <code>True</code>

Whereas <code>"abc" == ['a', 'c', 'b']</code> returns a <code>False</code> Boolean.

What does this tell us? Well, firstly it demonstrates that the String type within Haskell is equal to (I'm uncertain as to whether I would consider them to be equivalent, perhaps someone can clear that up for me?) a list of individual characters. Furthermore, it shows that the order of lists matter. All this is to say, we can do cool stuff like Constructing a string using an individual character (of type Char) by combining it with a String type, as is demonstrated below.

<code>'t' : "estnet"</code>

Then, we can perform a check to ensure it is indeed a string (because, WHY NOT!?)

<code>('t' : "estnet") == "testnet"</code>

Returns <code>True</code>.

### 2.1.2 Construct: Some Numbers!

<code>((1 : [1, 2]) : [[2, 3, 4]])</code>

Returns: <code>[[1,1,2],[2,3,4]]</code>

and <code>(((1 : [1, 2]) : [[2, 3, 4]]) : [[[4], [5], [6]]])</code>

Returns: <code>[[[1,1,2],[2,3,4]],[[4],[5],[6]]]</code>

*Can you see what's going on here?*

### 2.1.3 More Interesting Numbers & Lists

It is provable to create a list of any *length*, say from <code>x</code> to <code>n</code> using brackets (see below):

<code> 1 : (2 : (3 : []))</code>

Returns: <code>[1,2,3]</code>

### 2.2 Combining Lists: Using Append (++)

This is pretty straight forward, it's pretty similar to construct expect that the function declaration is a little different. Notice that this function accepts a list, rather than an element (as its first *and second* parameter):

<code>(++) :: [a] -> [a] -> [a]</code>

As you can clearly see, it also returns a list of exactly the same type. Thus, it simply concatenates the two lists together:

<pre><code>> day = [15]
> month = [09]
> year = [1993]
> birth = day ++ month ++ year
> birth
[15,9,1993]
</code></pre>

### 2.2.1 Using Append To Add An Element To The End Of A List

You cannot use Construct (:) to add an element to the end of a list, but sometimes this is something we might like to do. Thus, you use the following approach:

<code>[5,6,7,8] ++ [9, 10]</code>

Returns: <code>[5,6,7,8,9,10]</code>

### 2.3 List (And General) Scope Using Let & In

Within languages there are reserved keywords. In Haskell we can simply define a variable using <code>let</code> if we want, such as <code>let myList = [1,2]</code>, however we could just write <code>myList = [1,2]</code> - so why even bother with let? There is a very god reason...

**Scope is important** and is controlled (to a degree) by let and where you place let (is it within the **scope** of the current execution of the program? But, what is scope? Scope is the set of accessible memory locations within any given instance. Whether you're able to access a memory location that contains a variable or not is defined by the current scope of your program. You don't really want to create all your variables (which can also be functions) such that they're global, as you may want to access a previously defined 'x' or 'xs', but if you've defined new variables with the same name, well, you've lost your old ones! Both variables (with the same name) continue to exist in memory, but garbage collection works in the same fashion as you would typically expect (garbage collection meaning unused variables are 'freed' too allow for the new assignment of value to previously used, but now unused - since they're no longer in global scope - variables). We're not writing C, and we're not writing code for embedded devices (I don't think..?), so it's not hugely important to be able to free up memory super quick-time! Although, If I was sitting in these lectures I would probably ask: what process is responsible for garbage collection and how efficient is it? **NOTE: OFF-TOPIC**. Since the launch of Alonzo, we're finding that most people need at least 8GB of RAM to run a relay node, but 16GB is preferable. 16GB of RAM and 60+ GB of storage (and at least 2 CPUs) from a VPS provider (cloud instance) is costing me ~$240 a month, which, if I wasn't earning anything back from that, would be quite a bit of money! So I'm pretty interested in how we can optimise cardano-node, cardano SPs and perhaps even the Plutus compiler, though I have absolutely no idea how to write the target language (System F Omega with Recursive Type Classes?), albeit it would be interesting to know / learn how to optimise for the best time and space based computational complexity. With this said, I am simply a student of Haskell and Plutus right now, I just happen to be quite interested in compilers and have written A-LOT of assembly language (in ARM), so this is more: intellectual... I'm going to leave the next word out, but Linus Torvalds uses the phrase when discussing various processes that don't include contributing to the Kernel or writing C in a way that he approves of. Okay, I got a little off-topic here, but essentially, **we have Scope and when something is out of scope it is gobbled up by garbage collection in a typical manner.** 

So, we need a solution. We can use the reserved keyword <code>let</code> and then define a variable as we would do usually. However, if we follow that up with <code>in</code>, then the variable is only accessible within the <code>in</code> *clause*.

<pre><code>> let hello = ['h','e','l','l','o'] in hello
"hello"
> hello

<interactive>:40:1: error: Variable not in scope: hello
</code></pre>

The above illustrates the notion of scope.

### Recursion 

*A Recursive Definition: a list is either EMPTY, or constructed with an element and a list. Recursive functions in Haskell typically take the constructed form where you usually see something like: rL (x:xs) = x + rL xs, or where further functions are applied to the same pattern: rL xs = head xs : rL tail xs*

A list is either:

* Empty: []
* Constructed: x:xs
	* x is the head (an element), xs is the tail (a list).
	* remember that if xs := [a] then tail xs = [].
	* Further, if xs := [] then xs is the empty list (often the base case).
	* The null function is true if its argument is [].

Right, apparently recursion is so fantastic that we can use it to define "EVERYTHING A COMPUTER CAN DO!" - However, I must add a caviet (and a word of warning) -- sorry! Don't mean to spoil the party!

**Recursion is a fantastic way of solving problems. However, it can, on occasion, cause problems.** Computers implement executable code using a stack. If your recursive function does a little too much (or calls itself too many times), you run the risk of causing a stack overflow.

**Why? && How?**

Well, computers are discrete systems (this is why we study discrete mathematics, or at least, it's one reason why), as such they cannot hold the result of the first function call if the current function call in memory if it insists on additional memory to continue processing the recursive nature of its implementation. So, whilst recursion is actually quite a beautiful and eloquent it can be problematic.

*Note: I label it as beautiful because a lot of processes that exist within the phenomenal universe are recursive in nature, it's eloquent because it's the complete opposite of me! Thus, it expresses itself very concisely. However the planet, and mathematics are both continuous, meaning they allow for the concept of infinity, this is why we can run into things like Asymptotes in Mathematics. But again, computers are discrete, not infinite. So, recursion cannot implement everything a computer can do, within a computer (the ambiguity of language! Again, interesting stuff, go read authors from Phil of Mind and Phil of Language, I personally think computer scientists should draw from some of these disciplines, it's always good to have a little diversity in the way that you think IMO. Solving problems is creative, not always analytical. Apologies for the waffle.*

Thus, sometimes one has to take the route of practicality over beauty and eloquence! I leant this rather unfortunate principal both in university and in industry. We have to get the product out the door by the time we told clients we would, so we can't spend all day implementing things as perfectly as we would want to. This is called bad project management! Further, open source is a MUCH BETTER way of doing collective work, it's much more rewarding. I would encourage anyone reading these notes (I don't know why you would still be reading at this point, I must have already bored you to death) to participate in some kind of open source project! Anyway, the show must go on!

Oh, that seems to be the end. On to the next.