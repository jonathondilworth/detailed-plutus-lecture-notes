# More On Recursion

You can skip the big block of text if you're not interested in performance.

<hr />

**Somewhat off-topic, but it is introduced near the beginning of the lecture: How important is performance?**

Well, if we're writing in a fairly high level language, we likely shouldn't worry too much... Compilers can do a pretty decent job these days. As such, we can leave people who write compilers (and Linus Torvalds: [Youtube Video: "I worry about single instructions..."](https://youtu.be/dmfDaxYhi9I?t=1304)) to worry about micro-optimisation. However, if you're program is unnecessarily slow, perhaps there is a trade-off between clarity and making something that takes... I don't know, how about O(2^n) time complexity to run. If OTHER PEOPLE are going to be using your software, it becomes more of an issue... Electronjs instantly springs to mind, I've been told it's much better now, but I remember installing an IDE built on electronjs, and it was SLOW.

See the following Quora question as to [Why-are-desktop-apps-made-with-electron-js-framework-slow](https://www.quora.com/Why-are-desktop-apps-made-with-electron-js-framework-slow) - you can also review [their website for tips on how to improve performance](https://www.electronjs.org/docs/latest/tutorial/performance), is you feel so inclined.

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

*Authors question about readability, although <code>[x | x <- [1..100], x `mod` 3 == 0]</code> and the <code>enumFromToBy</code> function are not equivalent, they are doing more or less the same thing. Typically the way I think most people are taught when learning programming and implementing this kind of counting function is to use a modulo operator. As such, if I was actually taking this course as an undergraduate, how would a one liner function using a list comprehension and a modulo operator constraint be marked against the provided solution? Assuming enumFromToBy call called correctly and counted from zero instead of one. To me, it's pretty darn readable and even more concise: "generate a list of x where x is drawn from a list of integers between zero and one hundred, such that for each x, x must be completely divisible by 3", in fact, I find the enumFromToBy more difficult to read. Hang on a moment, I forgot we were talking about recursion...*

**A question that was asked: does Haskell have a limit on recursion?** Well yes and no, the compiler can't possibly know how deep a recursive call is going to continue for, can it? And we're running our code on a Turing Machine, thus the Halting Problem remains to be a problem.

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

* Apparently we can count forever? [1..], won't we eventually run out of memory? or since we do not need to remember any bound here, we can overwrite memory and continue to count (forever)?
* Brief discussion of zip, which is a mapping function, such that: zip [1..3] ['a', 'b', 'c'] returns three tuples within a list, where each value of the first list has been paired with each value from the second list. But you can also do cool stuff like: zip [0..] ['abcdefghijkjasjdhgahfdjanvs']
* (zip [1,2,3] ['a','b','c']) == (zip [1,2,3] "abc")
* zero-based indexing is good.
* ... onwards and upwards