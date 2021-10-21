### Algebraic Data Types

*I found this lecture a little more difficult, will need to rewatch it a couple of times. Some parts were really easy to grasp, other parts were a little difficult to understand. Likely because I'm a fool!*

We went through quite a bit during this lecture. In fact, I'm going to rewatch it at least another time as there were some elements that seemed somewhat puzzling. However, there were other areas of the lecture that seemed extremely digestible. Firstly, we start with a quick recap, there seemed to be no questions. Secondly, Algebraic Data Types are outlined which is shortly followed by a discussion of data, where does it come from? Furthermore, the concept of data declarations is introduced and we go on to talk about Constructors (as data 'terms'?) and other areas of interest such as lists (and their nature).

*Again, will need to rewatch this lecture and edit these notes appropriately. So for now, this document is a WORK IN PROGRESS.*

##### Recapping

* DATA! Numbers and Strings
* Functions over that data
* Recursion
* Comprehensions
* Higher Order Functions (Patterns which can be captured: .e.g. foldr)
* Phil is God (Delivered these data types from above! Gifted unto you!)

*Paused video.*

Where does the data come from? My intuition is: IO, right? Alternatively, you just define the values within your program, but that really does limit the number of potential use cases, right, or wrong? *I briefly read Imperative Functional Programming, am I correct in saying that the IO Monad essentially came out of that paper, or was it just optimisations for the compiler? I admit, I need to put more time into reading about the history of Haskell and I didn't give that paper the time it deserved, I briefly skimmed over it.*

*Unpaused video.*

Okay, I believe I got a little lost! No problem, it's all a learning process.

<hr />

Okay, so we're going to talk about data declarations and some more on functions. We're looking at Turing complete programs.

*Yes, I'm ready (:*

**ADTs**

* Bools
* Seasons
* Shapes
* Lists
* Natural Numbers [0, inf]
* Expressions
* Trees
* Maybe (Functor, or Monad?)
* Pair
* Either

#### Part Two: Booleans

data Bool = False | True

Operators: <code>Not</code>, <code>&&</code>, <code>||</code>

Essentially, no matter what data types you're dealing with, if they evaluate to a boolean expressions (or a function returns a boolean value) then:

* <code>&&</code> IFF False within expression THEN False ELSE True
* <code>||</code> IFF True within expression THEN True ELSE False
* <code>Not</code> Simply a negation of the localised expressions
	* <code>(not True) => False</code> <br />
	  <code>not (not True) => True</code>

Question Asked: Are there primitives within the language? Answer: Yes, furthermore the most important data types are Bools and Naturals.

*paused video*

Okay, so I ask myself why? Here is my intuition. If you can express everything using the NAND calculus (Booleans and the above operators on Bools), well you can create a formal language that extends to any computable function. I really don't have any intuition as to why the set of natural numbers is super important, I would have intuitively thought it would be set of real numbers? **These are just my thoughts! I may be super naive, I'm just trying to learn.**

*unpause*

Okay, I may be super stupid, I'm not sure I quite a followed the explanation... Again, I think I'll rewatch this later.

<hr />

This seems to be getting easier now...

Somewhat confused about the emphasis and focus on LHS and RHS of equations in Haskell. It seems to me to be fairly straight forward, consider:

```haskell
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect w h) = w * h
```

Everything is a function, even values. Just to make this explicit:

```haskell
Prelude> :t 2
2 :: Num p => p
```

The 'Number' 2 has a signature tied to it. So, the <code>area</code> function accepts a <code>Shape</code> as a parameter and returns a <code>Float</code>. After having declared the function signature, we are free to define the function (in fact, we're not even **required** to provide a function declaration, but it's considered good practice). Now, in this instance, both <code>Circle</code> and <code>Rect</code> are both Shapes, right? The reason we're encapsulating the <code>Circle</code> and <code>Rect</code> functions in brackets is because we want to evaluate those 'Constructors' first such that their return values can be fed to <code>area</code>. In exchange we get a floating point value (returned from area). Why? Because the signature for area is <code>Shape -> Float</code> so we're expecting a float.

*Am I missing something here?*

It may be worth pointing out that Shape is of type data. Thus, it can take the form of a <code>Circle</code> or a <code>Rect</code>. As tempting as it is to use the word template, it's defining a 'term' I believe.

*Worth remembering:*

**Constructors always begin with a capital letter, functions always begin with a small letter!**

### My Fav Data Type <3 LISTS (:

```haskell
data List a    =    Nil
               |    Cons a (List a)
               
append :: List a -> List a -> List a
append Nil ys            =  ys
append (Cons x xs) ys    =  Cons x (append xs ys)

(++) :: [a] -> [a] -> [a]
[] ++ ys        =   ys
(x:xs) ++ ys    =   x : (xs ++ ys)
```

*My intuition: lists are either empty or they Cons with an element or the head of another list, which is itself an element?*

