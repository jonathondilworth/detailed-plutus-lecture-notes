### A Lot Of Questions & Select, Take & Drop

*Variable Names: it's funny how during my UG at Manchester, we were told: very long descriptive variable names are GOOD. Guess what language we were using?*

*Monads are scary, and I thought Maybe was a Monad?*

Although it doesn't matter how much you indent, it may be quite important to indent neatly such that your program is readable. Furthermore, don't use tabs; or if you are very use to using tabs change the functionality of your IDE to convert tabs to spaces.

<hr />

#### Select, Take, Drop

*Operations on lists here...*

```haskell
Prelude> "words" !! 3 == 'd' -- zero based index, clean / bang operation on ['w','o','r','d','s']
True
Prelude> take 3 "words" == "wor"
True
Prelude> drop 3 "words"
"ds"
```
**Big emphasis on immutability!**

I'm not too sure how much this actually matters, but creating a new variable with the same 'referential name' as a previous variable doesn't change the value in memory where the old value was stored. Rather, a new value is placed into a new memory location and I assume it's just the pointer that changes? Then garbage collection takes over as you would normally expect. As such, so long as these 'variables' are within the same scope, they are re-issuable constants?

```haskell
Prelude> :{
Prelude| take :: Int -> [a] -> [a]
Prelude| take 0 xs       =  []
Prelude| take i []       =  []
Prelude| take i (x:xs)   =  x : take (i-1) xs
Prelude| :}
Prelude> take 10 [0..]
[0,1,2,3,4,5,6,7,8,9]
Prelude>
```


<hr />

*I don't particularly want to skip any content, but I feel as though I should probably go and actually write some Haskell programs, basically just sitting in GHCi and writing basic functions. e.g. I'll go and have a look at some stuff from Monday Morning Haskell. I think I've got a fairly good intuition regarding some of this early content. May come back to this...*