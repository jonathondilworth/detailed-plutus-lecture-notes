# Expression Trees as Algebraic Data Types & More

These notes are a collection of thoughts drawn from watching lectures nine, ten and reviewing some additional material by Simon Peyton Jones and various books as outlined within the resources file within this directory.

### Recap on Lecture 9

<details>

<summary>Note On Asking Questions</summary>

*Authors Note: Prof Wadlers statement about schools and not asking too many questions: I can relate to having attended a public secondary school in rural Wales. TLDR: it is a problem because I find it difficult to ask questions to this day... Working on it.*

</details>

I admit, I was scratching my head for a while thinking about all the information from the last lecture... I kept trying to do the following:

```haskell
Prelude> :t Lit -- uncertain of what it would return
Prelude> :t Prop -- thinking it would return some kind of reference to data
```
So, I went back and re-wrote the code, executed it, played around with it.

```haskell
Prelude Data.Eq> :t Lit

<interactive>:1:1: error: Data constructor not in scope: Lit
Prelude Data.Eq> :t Var

<interactive>:1:1: error: Data constructor not in scope: Var
Prelude Data.Eq> :{
Prelude Data.Eq| data Exp  =  Lit Int
Prelude Data.Eq|           |  Add Exp Exp
Prelude Data.Eq|           |  Mul Exp Exp
Prelude Data.Eq|           deriving (Eq, Show)
Prelude Data.Eq|
Prelude Data.Eq| evalExp :: Exp -> Int
Prelude Data.Eq| evalExp (Lit n)   =   n
Prelude Data.Eq| evalExp (Add e f) =   evalExp e + evalExp f
Prelude Data.Eq| evalExp (Mul e f) =   evalExp e * evalExp f
Prelude Data.Eq|
Prelude Data.Eq| showExp :: Exp -> String
Prelude Data.Eq| showExp (Lit n)   =   show n
Prelude Data.Eq| showExp (Add e f) =   par (showExp e ++ "+" ++ showExp f)
Prelude Data.Eq| showExp (Mul e f) =   par (showExp e ++ "*" ++ showExp f)
Prelude Data.Eq|
Prelude Data.Eq| par :: String -> String
Prelude Data.Eq| par s   =   "(" ++ s ++ ")"
Prelude Data.Eq| :}
Prelude Data.Eq> e0, e1 :: Exp

<interactive>:28:1: error:
    The type signature for ‘e0’ lacks an accompanying binding

<interactive>:28:5: error:
    The type signature for ‘e1’ lacks an accompanying binding
Prelude Data.Eq> :{
Prelude Data.Eq| e0, e1 :: Exp
Prelude Data.Eq| e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
Prelude Data.Eq| e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)
Prelude Data.Eq| :}
Prelude Data.Eq> show e0
"Add (Lit 2) (Mul (Lit 3) (Lit 3))"
Prelude Data.Eq> e0
Add (Lit 2) (Mul (Lit 3) (Lit 3))
Prelude Data.Eq> evalExp e0
11
Prelude Data.Eq> evalExp e1
15
Prelude Data.Eq>
```

This is what really brought it home for me:

```haskell
<interactive>:28:1: error:
    The type signature for ‘e0’ lacks an accompanying binding

<interactive>:28:5: error:
    The type signature for ‘e1’ lacks an accompanying binding
```

Exp is a Constructor (with a capital C). Duh. This did, however, lead me down somewhat of a rabbit hole as you'll see, shortly.

I do understand the concept of building an expression tree, similarly to building an abstract syntax tree; the difficulty for me is more about recognising what some of these language constructs do. You may ask the question: well why? You obviously know how a constructor works in an OOP language? Yeah. I suppose. Maybe it took some time to sink in? Maybe it's because I couldn't get my head around why ```:t Exp``` doesn't return a signature? Values and functions return signatures:

```haskell
Prelude Data.Eq> :t evalExp
evalExp :: Exp -> Int
Prelude Data.Eq> :t 2
2 :: Num p => p
```

Undefined Types do not return signatures (for obvious reasons, they're undefined - although, I thought Lit existed in Haskell?):

```haskell
Prelude Data.Eq> :t Lit

<interactive>:1:1: error: Data constructor not in scope: Lit
```
But after having having defined an ```Exp``` as having a base unit ```Lit``` (which is, in this case an ```Int```), we've brought Lit into scope (or existence)? *Not too sure on this point.*

```haskell
Prelude Data.Eq> :t Lit
Lit :: Int -> Exp
```
However, I guess I am still somewhat confused as to why ```:t Exp``` returns the following (even after having defined it as a data Constructor, I would have thought something would be returned, even if it's just: Exp :: Exp a => a -> a, since it's recursive and is essentially defined in terms of itself?):

```haskell
Prelude Data.Eq> :t Exp

<interactive>:1:1: error:
    • Data constructor not in scope: Exp
    • Perhaps you meant variable ‘exp’ (imported from Prelude)
Prelude Data.Eq>
```

*I am not the sharpest tool in the toolbox and often need a little time to digest things, so this is likely perfectly normal, esp. for me.*

Regardless, I'm not too perplexed (I would actually say that I'm tending towards understanding and moving further and further away from perplexity). Right, so let's do some explaining...

Given:

```haskell
Prelude Data.Eq> :{
Prelude Data.Eq| data Exp  =  Lit Int
Prelude Data.Eq|           |  Add Exp Exp
Prelude Data.Eq|           |  Mul Exp Exp
Prelude Data.Eq|           deriving (Eq, Show)
Prelude Data.Eq|
Prelude Data.Eq| evalExp :: Exp -> Int
Prelude Data.Eq| evalExp (Lit n)   =   n
Prelude Data.Eq| evalExp (Add e f) =   evalExp e + evalExp f
Prelude Data.Eq| evalExp (Mul e f) =   evalExp e * evalExp f
Prelude Data.Eq|
Prelude Data.Eq| showExp :: Exp -> String
Prelude Data.Eq| showExp (Lit n)   =   show n
Prelude Data.Eq| showExp (Add e f) =   par (showExp e ++ "+" ++ showExp f)
Prelude Data.Eq| showExp (Mul e f) =   par (showExp e ++ "*" ++ showExp f)
Prelude Data.Eq|
Prelude Data.Eq| par :: String -> String
Prelude Data.Eq| par s   =   "(" ++ s ++ ")"
Prelude Data.Eq| :}
Prelude Data.Eq> :{
Prelude Data.Eq| e0, e1 :: Exp
Prelude Data.Eq| e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
Prelude Data.Eq| e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)
Prelude Data.Eq| :}
```

Consider:

```haskell
Prelude Data.Eq> showExp e0
"(2+(3*3))"
```

Firstly, since ```showExp``` takes an Expression and returns a String, when recursively parsing an expression we're calling ourselves (showExp) whose base case is ```show n``` which is why we're able to use ```++``` to concatenate 'lists' together, since we're essentially converting an expression to it's base literal Int, to a string, as strings are lists of characters.

Secondly, the order of precedence is assigned through the use of brackets (as you would expect to see in many other languages; however it is pretty important here, as there would be no inherent order of precedence since these are our own operators defined as functions); using par in showExp, we're dropping the String ```(``` and ```)``` into the expression where the literal bracket is.

```evalExp``` works in exactly the same way as ```showExp``` except it is actually performs the calculations, it's not simply printing the expression to console.

**I really like how much emphasis is put on asking questions! You see this again and again: [Phil Wadler](https://media.ed.ac.uk/playlist/dedicated/179956591/1_0wugzt69/1_ihw6pzqo) (at 20:09) and [Simon P Jones](https://youtu.be/6COvD8oynmI?t=19) (How is Haskell BELOW ACTIONSCRIPT!?) during their lectures. Great culture within the world of Haskell.**

**For Funnzies, let's just add (no pun intended) Subtract and Divide:**

*IK, DRY, right? For the purposes of learning, I am going to repeat myself.*

```haskell
Prelude> :{
Prelude| data Exp  =  Lit Int
Prelude|           |  Add Exp Exp
Prelude|           |  Mul Exp Exp
Prelude|           |  Sub Exp Exp
Prelude|           |  Div Exp Exp
Prelude|           deriving (Eq, Show)
Prelude| evalExp :: Exp -> Int
Prelude| evalExp (Lit n)   =   n
Prelude| evalExp (Add e f) =   evalExp e + evalExp f
Prelude| evalExp (Mul e f) =   evalExp e * evalExp f
Prelude| evalExp (Sub e f) =   evalExp e - evalExp f
Prelude| evalExp (Div e f) =   evalExp e `div` evalExp f -- note: infix
Prelude| showExp :: Exp -> String
Prelude| showExp (Lit n)   =   show n
Prelude| showExp (Add e f) =   par (showExp e ++ "+" ++ showExp f)
Prelude| showExp (Mul e f) =   par (showExp e ++ "*" ++ showExp f)
Prelude| showExp (Sub e f) =   par (showExp e ++ "-" ++ showExp f)
Prelude| showExp (Div e f) =   par (showExp e ++ "/" ++ showExp f)
Prelude| par :: String -> String
Prelude| par s   =   "(" ++ s ++ ")"
Prelude| :}
Prelude> :{
Prelude| e0, e1 :: Exp
Prelude| e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
Prelude| e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)
Prelude| :}
Prelude> :{
Prelude| e2, e3 :: Exp
Prelude| e2 = Add (Lit 2) (Div (Lit 3) (Lit 3))
Prelude| e3 = Div (Add (Lit 3) (Lit 3)) (Lit 3)
Prelude| :}
Prelude> evalExp e2
3
Prelude> evalExp e3
2
Prelude> evalExp e0
11
Prelude> evalExp e1
15
Prelude> :{
Prelude|  e4, e5 :: Exp
Prelude|  e4 = Mul (Lit 1) (Div (Lit 1) (Lit 2))
Prelude|  e5 = Div (Div (Lit 1) (Lit 2)) (Lit 1)
Prelude| :}
Prelude> evalExp e4
0
Prelude> evalExp e5
0
Prelude> -- because they're ints!
Prelude> -- confused for a sec! Could change base case to Lit Num?
Prelude> showExp e4
"(1*(1/2))"
Prelude> showExp e5
"((1/2)/1)"
Prelude>
```

Righhhhtt, I get it now (I think, I was nodding earlier, but it's slowly becoming clearer and clearer). Thanks Phil.

Let's try and make some sense of this then...

Recursive data definitions VS recursive function definitions. So... Recursive data definitions don't have types, because they're (essentially, or result in) structures? They do, however, have functions (you need to be able to apply operations to them), thus you can pull a type signature off a recursive data function, but not a recursive data definition? Furthermore, a recursive data definition is parsable as there is an atomic unit which represents its... identity? And you can create functions to parse it which eventually reach a base case.

<small>*Thus, you could actually implement a language like this? Define a data definition, allow users to create inputs using such a definition, so long as you have defined functions to parse it, label it and evaluate it. This is what DSLs are?*</small>

### Back to: Lecture 10 / End of L9

*Propositions as Types*


```haskell
type Name = String -- constructors start with a capital letter
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop -- Since symbols which are constructors cannot start with a
          | Prop :&: Prop -- 'capital letter', convention is to wrap in :<SYMB>:
          deriving (Eq, Show)
          
type Names = [Name]
type Env = [(Name, Bool)]
```

**notes on: Var**

> Var is a synonym for the Id type but it may additionally potentially contain type variables, which have a Kind rather than a Type and only contain some extra details during type-checking.
These Var names may either be global or local, see Var

> Global Ids and Vars are those that are imported or correspond to a data constructor, primitive operation, or record selectors. Local Ids and Vars are those bound within an expression (e.g. by a lambda) or at the top level of the module being compiled.

See: [https://hackage.haskell.org/package/ghc-8.10.2/docs/Var.html](https://hackage.haskell.org/package/ghc-8.10.2/docs/Var.html)

**Back To Haskell Snippet**

* Anything of Type Name is a String
* We define a data type called Prop (for proposition)
* A Propositions Base Case is: ```Var Name``` which is a ```String```
* Propositions contain Boolean values (why isn't the base case defined as a (Name, Bool)?)
* Propositions are built of:
* F (for False)
* T (for True)
* Not (for negate)
* Prop :|: Prop (Or Constructor)
* Prop :&: Prop (And Constructor)
* deriving Eq means we can test for equality
* Show is there too, but we're going to implement our own showProp

**The Rest Of The Implementation**

```Haskell
Prelude Data.List> import Data.List
Prelude Data.List> :t nub
nub :: Eq a => [a] -> [a]
Prelude Data.List> :{
Prelude Data.List| type Name = String -- constructors start with a capital letter
Prelude Data.List| data Prop = Var Name
Prelude Data.List|          | F
Prelude Data.List|          | T
Prelude Data.List|          | Not Prop
Prelude Data.List|          | Prop :|: Prop -- Since symbols which are constructors cannot start with a
Prelude Data.List|          | Prop :&: Prop -- 'capital letter', convention is to wrap in :<SYMB>:
Prelude Data.List|          deriving (Eq, Show)
Prelude Data.List|
Prelude Data.List| type Names = [Name]
Prelude Data.List| type Env = [(Name, Bool)]
Prelude Data.List|
Prelude Data.List| showProp :: Prop -> String
Prelude Data.List| showProp (Var x)    =   x
Prelude Data.List| showProp (F)        =   "F"
Prelude Data.List| showProp (T)        =   "T"
Prelude Data.List| showProp (Not p)    =   par ("~" ++ showProp p)
Prelude Data.List| showProp (p :|: q)  =   par (showProp p ++ "|" ++ showProp q)
Prelude Data.List| showProp (p :&: q)  =   par (showProp p ++ "&" ++ showProp q)
Prelude Data.List|
Prelude Data.List| par :: String -> String
Prelude Data.List| par s   =   "(" ++ s ++ ")"
Prelude Data.List|
Prelude Data.List| names :: Prop -> Names
Prelude Data.List| names (Var x)       =   [x]
Prelude Data.List| names (F)           =   []
Prelude Data.List| names (T)           =   []
Prelude Data.List| names (Not p)       =   names p
Prelude Data.List| names (p :|: q)     =   nub (names p ++ names q)
Prelude Data.List| names (p :&: q)     =   nub (names p ++ names q)
Prelude Data.List|
Prelude Data.List| evalProp :: Env -> Prop -> Bool
Prelude Data.List| evalProp e (Var x)  =   lookUp e x
Prelude Data.List| evalProp e (F)      =   False
Prelude Data.List| evalProp e (T)      =   True
Prelude Data.List| evalProp e (Not p)  =   not (evalProp e p)
Prelude Data.List| evalProp e (p :|: q) =  evalProp e p || evalProp e q
Prelude Data.List| evalProp e (p :&: q) =  evalProp e p && evalProp e q
Prelude Data.List|
Prelude Data.List| lookUp :: Eq a => [(a,b)] -> a -> b
Prelude Data.List| lookUp xys x   =   the [y | (x',y) <- xys, x == x']
Prelude Data.List|   where
Prelude Data.List|   the [x]   =   x
Prelude Data.List|
Prelude Data.List| -- given a list of tuples (a,b) -- the env -- return b, given a
Prelude Data.List| -- surly this could be implemented using a mapping function?
Prelude Data.List|
Prelude Data.List| :}
Prelude Data.List> pp0 :: Prop

<interactive>:110:1: error: Variable not in scope: pp0 :: Prop
Prelude Data.List> :{
Prelude Data.List| pp0 :: Prop
Prelude Data.List| pp0   =   (Var "i" :|: Not (Var "i")) :&: (Var "a")
Prelude Data.List| :t pp0
Prelude Data.List| pp0
Prelude Data.List| names pp0
Prelude Data.List| :t names
Prelude Data.List| :}

<interactive>:114:1: error: parse error on input ‘:’
Prelude Data.List> :{
Prelude Data.List| p0 :: Prop
Prelude Data.List| p0 = (Var "a" :&: Var "b) :|: (Not (Var "a") :&: Not (Var "b"))
Prelude Data.List| :}

<interactive>:115:64: error:
    lexical error in string/character literal at end of input
Prelude Data.List> :t Prop

<interactive>:1:1: error:
    • Data constructor not in scope: Prop
    • Perhaps you meant variable ‘drop’ (imported from Prelude)
Prelude Data.List> :t Name

<interactive>:1:1: error: Data constructor not in scope: Name
Prelude Data.List> :t Names

<interactive>:1:1: error:
    • Data constructor not in scope: Names
    • Perhaps you meant variable ‘names’ (line 86)
Prelude Data.List> :t names
names :: Prop -> Names
Prelude Data.List> pp1 :: Prop

<interactive>:119:1: error: Variable not in scope: pp1 :: Prop
Prelude Data.List> :{
Prelude Data.List| pp2 :: Prop
Prelude Data.List| pp2 = "h"
Prelude Data.List| :}

<interactive>:122:7: error:
    • Couldn't match expected type ‘Prop’ with actual type ‘[Char]’
    • In the expression: "h"
      In an equation for ‘pp2’: pp2 = "h"
Prelude Data.List> :{
Prelude Data.List| pp3 :: Prop
Prelude Data.List| pp3 = (Var "h")
Prelude Data.List| :}
Prelude Data.List> pp3
Var "h"
Prelude Data.List> showProp pp3
"h"
Prelude Data.List> evalProp [("h", True), ("b", False)] h

<interactive>:130:38: error: Variable not in scope: h :: Prop
Prelude Data.List> evalProp [("h", True), ("b", False)] pp3
True
Prelude Data.List> :t (:)
(:) :: a -> [a] -> [a]
Prelude Data.List> :t (:&:)
(:&:) :: Prop -> Prop -> Prop
Prelude Data.List> :t :&:

<interactive>:1:1: error: parse error on input ‘:&:’
Prelude Data.List> -- hmmm, maybe if we put brackets around the OR and AND constructors?
Prelude Data.List> -- it seems to me that (:&:) and (:|:) are being tokenised such that (:) is being
Prelude Data.List> -- read as a single token?
Prelude Data.List>
Prelude Data.List> :{
Prelude Data.List| ppp0 :: Prop
Prelude Data.List| ppp0 = (Var "a" (:&:) Var "b")
Prelude Data.List| :}

<interactive>:141:9: error:
    • Couldn't match expected type ‘(Prop -> Prop -> Prop)
                                    -> (Name -> Prop) -> [Char] -> Prop’
                  with actual type ‘Prop’
    • The function ‘Var’ is applied to four arguments,
      but its type ‘Name -> Prop’ has only one
      In the expression: (Var "a" (:&:) Var "b")
      In an equation for ‘ppp0’: ppp0 = (Var "a" (:&:) Var "b")
Prelude Data.List> -- Maybe not?
Prelude Data.List>
Prelude Data.List> :{
Prelude Data.List| ppp1 :: Prop
Prelude Data.List| ppp1 = (Var "a" :&: Var "b")
Prelude Data.List| :}
Prelude Data.List> ppp1
Var "a" :&: Var "b"
Prelude Data.List> -- that seems to work...
Prelude Data.List>
Prelude Data.List> evalProp ppp1

<interactive>:152:10: error:
    • Couldn't match type ‘Prop’ with ‘[(Name, Bool)]’
      Expected type: Env
        Actual type: Prop
    • In the first argument of ‘evalProp’, namely ‘ppp1’
      In the expression: evalProp ppp1
      In an equation for ‘it’: it = evalProp ppp1
Prelude Data.List> showProp ppp1
"(a&b)"
Prelude Data.List> :{
Prelude Data.List| ppp2 :: Prop
Prelude Data.List| ppp2 = (Var "a" :&: Var "b") :|: (Not (Var "a") :&: Not (Var "b"))
Prelude Data.List| :}
Prelude Data.List> ppp2
(Var "a" :&: Var "b") :|: (Not (Var "a") :&: Not (Var "b"))
Prelude Data.List>
Prelude Data.List> -- must have mistyped something somewhere!
Prelude Data.List> showProp ppp2
"((a&b)|((~a)&(~b)))"
Prelude Data.List> evalProp [("a", False), ("b", False)] ppp2
True
Prelude Data.List> -- yup, (false and false) or (not false and not false), since not false is true, and or between false and false, etc...
Prelude Data.List> -- need to implement Envs for loopup...
Prelude Data.List> -- lookUp*
Prelude Data.List> :{
Prelude Data.List| envs :: Names -> [Env]
Prelude Data.List| envs []       =  [[]] -- wouldn't it be an empty list of tuples?
Prelude Data.List| envs (x:xs)   =  [ (x,b):e | b <- bs, e <- envs xs ]
Prelude Data.List|   where
Prelude Data.List|   bs   =   [False, True]
Prelude Data.List|
Prelude Data.List| -- Okay, I'm a little lost here, I think I'll resume the lecture...
Prelude Data.List| :}
Prelude Data.List> env0 = [("a", False), ("b", False)]
Prelude Data.List> :t env0
env0 :: [([Char], Bool)]
Prelude Data.List> -- the list comprehension is somewhat confusing, ohhh.. we're passing envs as a list, yeah this makes sense
Prelude Data.List> evalProp en0 ppp2

<interactive>:195:10: error:
    • Variable not in scope: en0 :: Env
    • Perhaps you meant ‘env0’ (line 192)
Prelude Data.List> evalProp env0 ppp2
True
Prelude Data.List> -- okay, time for a break...
Prelude Data.List>
Prelude Data.List>
Prelude Data.List> -- REALLY GOOD explaination for the list comprehension:
Prelude Data.List> -- take for example (x,y) s.t x (and y) is drawn from...
Prelude Data.List> [(x,y) | x <- [1,2,3], y <- [1,2,3]]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
Prelude Data.List> [(x,y) | x <- [1,2,3], y <- [1,2]]
[(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)]
Prelude Data.List> [(x,y) | x <- [1,2,3], y <- [1]]
[(1,1),(2,1),(3,1)]
Prelude Data.List> [(x,y) | x <- [1,2,3], y <- []]
[]
Prelude Data.List>
Prelude Data.List>
Prelude Data.List> -- still...
Prelude Data.List> :t ("a", False)
("a", False) :: ([Char], Bool)
Prelude Data.List> -- ahhh, the base case for envs [] is [[]] because you're literally passing a list of strings, strings are list
Prelude Data.List> -- thus, list of lists
Prelude Data.List> -- quite a bit to take in for L9 and  L10
Prelude Data.List> -- when you pass in strings, you're generating all possible environments
```

*Some Additional Materials*

```haskell
Prelude> :{
Prelude| data Exp = Lit Int
Prelude|          | Add Exp Exp
Prelude|          | Div Exp Exp
Prelude|          deriving (Show, Eq)
Prelude|
Prelude| evalE :: Exp -> Int
Prelude| evalE (Lit n)   =   n
Prelude| evalE (Add p q) =   evalE p + evalE q
Prelude| evalE (Div p q) =   evalE p `div` evalE q
Prelude| :}
Prelude> :{
Prelude| e :: Exp
Prelude| e = Div (Lit 3) (Add (Lit 4) (Lit 4))
Prelude| :}
Prelude> evalE e
0
Prelude> -- why zero? Because Lit Int (Int, not Num)
```

![../img/expt.JPG](../img/expt.JPG)

*You create your own luck.*

**You should test.**