### Quick Recap!

**Functions**

Best practice is to (I think it's declare? then define?):

pd :: Int -> Int -> Int -- dec <br />
pd x y = x * y -- def <br />

fairly standard stuff.

**Lists**

Strings, [Char], String == [Char], Num [a] ... Lists can only contain one type, however, if that type is say a tuple, you can have a list of tuples, where each tuple is a an Int and a String, for example.

**List Comps**

Essentially generators, where you say, each element of the list is a value given that it's drawn from some expression, given a possible guard and constraints.

**Mappings**

Map takes two formal parameters, the first is a function and the second is typically a list.

If the input to the first formal parameter of map (the input to the referenced function) is of a polymorphic type to the elements within the list (the second formal parameter), map will proceed to apply that function to each element within the list.

**foldl, foldr**

Evaluating a function and 'rolling it' from either left to right or right to left.

**Curried Functions**

Taking as input (to a function) another functions output.

**Function Mappings, fmap, functors**

fmap is similar to map, except it's mapping functions to functions. We're getting closer to Monads (it feels?).

fmap can be written shorthand: with DOT or <\$\>.

**Polymorphism and Parametric Polymorphism**

Polymorphism, I think is like the type Num, it can take on the role of an Int, Integer, Float, Double, etc.

Parametric Polymorphism is... Essentially the same thing, right? I mean, it just predated the word 'generics' in OOP? It's a type (class) which implements the same 'functionality' when given as a parameter (to a function) such that the type can be implicitly defined within a given function. Issues are resolved at compile time?

<hr />

*After a single parse, I feel quite comfortable with the above, also I like Type Classes and Data Constructors. Recursion is a little more difficult... I prefer list comps. Monoids made sense, but I'm still processing; Monads, again, >>=, >>, I'm still kind of processing... Scope is fine, like do notation is fine. Building a project is still... difficult! cabal, stack, etc - we didn't get to know too much about tooling I don't think during this intro to FP. I just need to let it all sink in a bit more and play around with some stuff.*

### Some Example...

```haskell
Prelude> -- functions!
Prelude> :{
Prelude| pd :: Int -> Int
Prelude| pd n = n * n
Prelude| :}
Prelude> pd 7
49
Prelude> -- List (& Comps)
Prelude> :{
Prelude| i = [ x | x <- [1..100], x `mod` 2 == 0]
Prelude| :}
Prelude> i
[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]
Prelude> -- Mappings
Prelude> map pd i
[4,16,36,64,100,144,196,256,324,400,484,576,676,784,900,1024,1156,1296,1444,1600,1764,1936,2116,2304,2500,2704,2916,3136,3364,3600,3844,4096,4356,4624,4900,5184,5476,5776,6084,6400,6724,7056,7396,7744,8100,8464,8836,9216,9604,10000]
Prelude> -- foldl, foldr
Prelude> foldl (+) 0 (map pd i)
171700
Prelude> -- Curried Functions
Prelude> :{
Prelude| foldlAdd :: [Int] -> Int
Prelude| foldlAdd n = foldl (+) 0 n
Prelude| :}
Prelude> :{
Prelude| mapPd :: [Int] -> [Int]
Prelude| mapPd n = map pd n
Prelude| :}
Prelude> foldlAdd (mapPd i)
171700
Prelude> -- function mappings
Prelude> (fmap foldlAdd mapPd) i
171700
Prelude> -- shorthand (functors?)
Prelude> (foldlAdd <$> mapPd) i
171700
Prelude> (foldlAdd . mapPd) i
171700
```

