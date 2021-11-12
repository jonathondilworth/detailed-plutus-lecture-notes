### Lecture 11, 12, 13, 14, ...

*Watched all the lectures now. Need to make better notes and rewatch IO and Monads. Although, great explanations!*

A question was asked: What is the difference between Data and Types? **Good Question!**

*It's a good question because I was also somewhat confused about this, more so to do with why Data CONSTRUCTORS fail to return any kind of signature, as far as I can tell. But of course, types do. I mean, come to think of it, it's probably because they're called TYPE SIGNATURES RIGHT!? Duh.*

> "Data is a value, like three. What would the type of three be? Int (well, perhaps Num, but yeah); what's the difference between three and Int? That's the difference between data and types."

**Challenge**

Write a Haskell program which implements graphics! I would be down for this, but I won't post any solutions online, as I have been requested not to do so.

**Let's go ahead and just skip Big O notation...**

### IT'S TYPE TIME (:

*Oh. Set Theory.*

Let's just have a quick look at the code, and maybe do some playful things in GHCi :)

**Sets as Lists!? Don't we have a built in Type for Set in Haskell?**

Of course we do, but let's just have some fun.

```haskell
Prelude> import Test.QuickCheck
Prelude Test.QuickCheck> type Set a = [a]
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| empty :: Set a
Prelude Test.QuickCheck| empty =  []
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| insert :: a -> Set a -> Set a
Prelude Test.QuickCheck| insert x xs =  x:xs
Prelude Test.QuickCheck| set :: [a] -> Set a
Prelude Test.QuickCheck| set xs = xs
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| element :: Eq a => a -> Set a -> Bool
Prelude Test.QuickCheck| x `element` xs     =    x `elem` xs
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| equal :: Eq a => Set a -> Set a -> Bool
Prelude Test.QuickCheck| xs `equal` ys = xs `subset` ys && ys `subset` xs
Prelude Test.QuickCheck|   where
Prelude Test.QuickCheck|   xs `subset` ys = and [ x `elem` ys | x <- xs ]
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| prop_element :: [Int] -> Bool
Prelude Test.QuickCheck| prop_element ys  =  
Prelude Test.QuickCheck|   and [ x `element` s == odd x | x <- ys ]
Prelude Test.QuickCheck|   where
Prelude Test.QuickCheck|   s = set [ x | x <- ys, odd x ]
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| check =
Prelude Test.QuickCheck|   QuickCheck prop_element
Prelude Test.QuickCheck| :}

<interactive>:34:3: error:
    • Data constructor not in scope: QuickCheck :: ([Int] -> Bool) -> t
    • Perhaps you meant variable ‘quickCheck’ (imported from Test.QuickCheck)
Prelude Test.QuickCheck> :{
Prelude Test.QuickCheck| check =
Prelude Test.QuickCheck|   quickCheck prop_element
Prelude Test.QuickCheck| :}
Prelude Test.QuickCheck> check
+++ OK, passed 100 tests.
Prelude Test.QuickCheck> 
```

Not exactly super efficient when using *elem*, essentially a nested loop as two list comprehensions, right? But, the compiler is going to optimise (hopefully?). O(n^2), right?

Oh. Ermm...? Pretty sure checking that an element exists from one list(..set?) in another is quadratic? Is there some special sauce in the compiler that speeds this up?

Ah. It is Quadratic, thank god.

Essentially, there are instances where you're going to use different implementations of sets (you'd probably want to analyse your programme to see what exactly it is you're doing, then use the appropriate implementation; or, you know, use whatever the standard library functions are / highly rated modules).

We go on to talk about Trees and... Well, more Data Structures (Balanced Trees and... Unbalanced Trees?). It may be interesting to implement something a bit different, a stack? Well, a stack is essentially an ordered list (not an ordered set), with push and pop (insert and remove); so we've kind of done that.

*This is where I begin to get ahead of myself...*

<hr />

*Let's try something a little more difficult! Let's go for a graph. In fact, let's pull a graphics library in and generate an ER Random Network, with the constraints defined by the Barabási–Albert model. A scale-free network whose degree distribution follows a power law: $$ f(j,x)\ \sim\  j^{-x}\ \ |\ \ \ j > 0,\ x > 0 $$... Perhaps we're biting off a little more than we can chew, but why not! Whilst we are at it, why don't we also include a function to calculate the global clustering co-efficient?*

**The Barbasi Albert Model**

An algorithm that generates a random scale free network with a preferential bias insofar as: for a network of size M, the following constraint must be met (given the probability that the new node will generate an edge to any other node based on each nodes degree, for all nodes) must be met:

$$ f(k_{i})\ = \def\specialFrac#1#2{\frac{k_{i}}{#1#2}} \specialFrac{\sum_{j=1}^{M}}{k_{j}} $$

Thus, any additional nodes are more likely to connect to existing nodes with a higher degree of connectivity. In addition, calculating the clustering co-efficient for any node within the network is defined by:

$$ C_{i} = 2\ \times \def\specialFrac#1#2{\frac{e_{i}}{#1\ #2}} \specialFrac{k_{i}}{(k_{i} - 1)} $$

Finally, the average density of clusters across the entire network can also be calculated:

$$ C = \frac{1}{M}\ \sum_{i}^{M}\ c_{i}\ \ |\ \ \forall\ i \in  K $$

Right and now for the hard part?

* A graph must have at least one node (not necessarily)
* Typically a set of nodes
* Each node has edges to other nodes
* Nodes can be numbered as Ints
* Given any node, its edges can be a list of Ints
* Giving us something like this:

```haskell
[[0, [1, 2, 4, 5]], [1, [0, 4]], [2, [1]], [4, [0, 1, 5]], [5, [1]]]
-- really what you want is:
[(0, [1, 2, 4, 5]), (1, [0, 4]), (2, [1]), (4, [0, 1, 5]), (5, [1])]
-- a list of tuples, where each tuple is an index and a list
-- technically, you could implement an undirected graph as a tree...
-- but there has to be a better way if you want an edge list for each node...
```

*I'm just going to leave a note here: come back and try this again soon!*

Right, so Phil Wadler is saying that if I use my brain I can figure this out. Let's harness that brain power!

*TO DO: IMPLEMENT THIS.*