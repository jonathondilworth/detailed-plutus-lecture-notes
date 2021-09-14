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

When it comes to scope (which essentially means whether you're able to access a memory location that contains a variable, a function, something like that. You don't really want to create all your variables such that they're global, it means that whenever you want to use the same name again, for example <code>xs</code>, we then loose our old <code>xs</code> (and it gets spewed up by garbage collection).

So, we need a solution. We can use the reserved keyword <code>let</code> and then define a variable as we would do usually. However, if we follow that up with <code>in</code>, then the variable is only accessible within the <code>in</code> *clause*.

<pre><code>> let hello = ['h','e','l','l','o'] in hello
"hello"
> hello

<interactive>:40:1: error: Variable not in scope: hello
</code></pre>



### Recursion 

A list is either:

* Empty: []
* Constructed: x:xs
	* x is the head (an element), xs is the tail (a list).
	* *(To Be Continued)*


