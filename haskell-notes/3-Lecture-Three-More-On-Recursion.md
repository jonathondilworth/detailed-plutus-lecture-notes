# More On Recursion

You can skip the big block of text if you're not interested in performance.

<hr />

**Somewhat off-topic, but it is introduced near the beginning of the lecture: How important is performance?**

Basically just me ranting about electronjs. Not sure how I got here, it's been a long weekend.

<hr />

### Right, Recursion (again).

[1..3] uses a function with signature: (Num a, Enum a) => [a] called **enumFromTo**, which is (apparently) *syntactic sugar*. 

Really, it's abstraction. In this case, we're using abstraction to remove an explicit function call. *Weren't we just talking about clarity!?*

According to the lecture, the implementation for enumFromTo is:

<pre><code>enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | m > n = []
               | m <= n = m : enumFromTo (m+1) n
</pre></code>

Counting by varying intervals. List comprehension with a function, or defining another type of function like enumFromTo(By):

<pre><code>Prelude> -- interesting to note that this was my intuition when I paused the video:
Prelude> c3 = [x | x <- [1..100], x `mod` 3 == 0]
Prelude> c3
[3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
 --
Prelude> -- However, when you implement the following function:
Prelude> :{
Prelude| enumFromToBy :: Int -> Int -> Int -> [Int]
Prelude| enumFromToBy from to by | from > to   =  []
Prelude|                         | from <= to  =  from : enumFromToBy (from + by) to by
Prelude| :}
Prelude> -- you get a behaviour I didn't expect, which is to say they're not equivalent functions
Prelude> enumFromToBy 1 100 3
[1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97,100]
Prelude> enumFromToBy 0 100 3
[0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
Prelude> -- I thought this was worth pointing out
</code></pre>

*Removed stupidity*

**A question that was asked: does Haskell have a limit on recursion?** It goes until you're OOM (out of memory, not out of mana).

<hr />

* The phrase 'unit' is equivalent to 'identity'
* Often 'helper' functions are required in order to 'remember' function bounds.
* This is helpful (and a common pattern) because is saves us from defining two global functions.

<pre><code>Prelude> :{
Prelude| factRec :: Integer -> Integer
Prelude| factRec n = fact 1 n
Prelude|   where
Prelude|   fact :: Integer -> Integer -> Integer
Prelude|   fact m n | m > n   =  1
Prelude|            | m <= n  =  m * fact (m+1) n
Prelude| :}
Prelude> factRec 5
120
Prelude> :{
Prelude| fact :: Integer -> Integer -> Integer
Prelude| fact m n | m > n   =  1
Prelude|          | m <= n  =  m * fact (m+1) n
Prelude| :}
Prelude> :{
Prelude| facto :: Integer -> Integer
Prelude| facto n = fact 1 n
Prelude| :}
Prelude> facto 5
120
Prelude> factRec 5 == facto 5
True
</code></pre>

* Apparently we can count forever? [1..], won't we eventually run out of memory? I'll take it on faith!
* Brief discussion of zip, which is a mapping function, such that: zip [1..3] ['a', 'b', 'c'] returns three tuples within a list, where each value of the first list has been paired with each value from the second list. But you can also do cool stuff like: zip [0..] ['abcdefghijkjasjdhgahfdjanvs']
* (zip [1,2,3] ['a','b','c']) == (zip [1,2,3] "abc")
* zero-based indexing is good.
* ... onwards and upwards