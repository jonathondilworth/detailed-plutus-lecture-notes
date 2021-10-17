### Fold, Right & Left!

**Additional emphasis on Zero Based Indexing. Fairly sure I've got it (:**

#### Fold Right

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f u []              =   u
foldr f u (x:xs)          =   x `f` (foldr f u xs)
```
<small>*Typo in the slides:* <code>-- foldr f u (x:xs)          =   x `f` (foldr f v xs)</code></small>

Just to expand on what exactly is happening here:

*In short, we're recursively applying a function specified by f to the second argument and the last items of the list, until we reach our, I'm tempted to say base case? However, given the lectures I'm airing on the side of: until we reach the identity? This mathematical nomenclature is frying my brain, I may context switch for now and review some of the Blockchain lectures. Anyway, I've implemented a demonstration below (evaluating foldr and foldl can result in different values due to, .e.g. if you use (/) ... Fancy math words, escaping the reach of my conscious mind).*

```haskell
Prelude> -- for example:
Prelude> foldr (/) 4 [16,24,48,8]
16.0
Prelude> (/) 24 16
1.5
Prelude> (/) 48 1.5
32.0
Prelude> (/) 8 32
0.25
Prelude> (/) 4 0.25
16.0
Prelude> -- just for kicks:
Prelude> foldl (/) 4 [16,24,48,8]
2.712673611111111e-5
Prelude>
```

