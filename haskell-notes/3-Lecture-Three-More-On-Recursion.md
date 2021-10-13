# More On Recursion

You can skip the big block of text if you're not interested in performance or *reflections on trusting trust!*

<hr />

**Somewhat off-topic, but it is introduced near the beginning of the lecture: How important is performance?**

Well, you're reading my notes, and you can likely tell that I am somewhat verbose, so I'm afraid you'll have to suffer a degree of 'pondering' as my friend 'Quasar' from the IOG discord server would put it. -- sorry!

Well, if we're writing in a fairly high level language, we likely shouldn't worry too much... Compilers can do a pretty decent job these days. As such, we can leave people who write compilers (and Linus Torvalds) to worry about micro-optimisation. However, if you're program is unnecessarily slow, perhaps there is a trade-off between clarity and making something that takes... I don't know, how about O(2^n) time complexity to run. If OTHER PEOPLE are going to be using your software, it becomes more of an issue... Electronjs instantly springs to mind, I've been told it's much better now, but I remember installing an IDE built on electronjs, and it was SLOW.

See the following Quora question as to [Why-are-desktop-apps-made-with-electron-js-framework-slow](https://www.quora.com/Why-are-desktop-apps-made-with-electron-js-framework-slow) - you can also review [their website for tips on how to improve performance](https://www.electronjs.org/docs/latest/tutorial/performance), is you feel so inclined.

**Even more off-topic:** I believe the Daedalus wallet is also implemented on top of electron. **Even even more off-topic:** When you download Deadalus, you get cardano-node too, so I assume you also get Plutus-tx for interfacing with smart contracts? I'm curious as to where the compiler for Plutus exists (for off-chain code to push a redeemer to a smart contract address). If there is an IR during the compilation pipeline from Plutus to Plutus-core, does this make the compiler vulnerable to attacks such as those outlined in reflections on trusting trust by Thompson? Esp. since an IR source language is similar it's target language (and both are difficult to read and write, System F Omega), could a routine not exist within a fake wallet to inject a malicious IR into the compilation pipeline? *I am by no means an expert, but I'm just sort of, thinking out loud. If this is a valid consideration, has it been thought about?* Assumptions: somebody downloaded a fake wallet. References: [Unraveling Recursion: Compiling an IR with Recursion to System F](https://homepages.inf.ed.ac.uk/wadler/papers/mpc-2019/unraveling.pdf) and [Reflections on Trusting Trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf).

**WOWZERS. We got pretty seriously off-topic.**

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

**A question that was asked: does Haskell have a limit on recursion?** Well yes and no, the compiler can't possibly know how deep a recursive call is going to continue for *(although, I have spent some time trying to understand an article telling me that computation based on Lambda calculus makes this somehow possible? Okay, well I did pure CS, not CS and Mathematics, so I don't know what to make of this claim, can the compiler catch a potentially infinite recursive call at compile time?)*. And we're running our code on a Turing Machine, thus the Halting Problem remains to be a problem.

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