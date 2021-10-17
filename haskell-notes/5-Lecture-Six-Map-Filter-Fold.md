### L6. Map, Filter & Fold

Okay, so after having quickly reviewed the contents of the lectures moving forward, it appears that there is a lot of material that may benefit me. Thus, I have decided to try and start getting one lecture per day down in notes. The longer I'm spending on learning Haskell, the easier it is becoming (would you have guessed!).

Within this lecture, firstly we're going to quickly discuss some administrative stuff and some armchair philosophy. Secondly, the whole point of programming is outlined to us by the lecturer. Shortly thereafter a brief discussion on the history of mathematics, computation, language, formal systems and logic is presented (in addition to some references which may be interesting). Furthermore, we recap on List Comprehensions and Recursive Functions which is shortly followed by an introduction to the set of library functions: Map, Filter and Fold. Within these later sections there is some discussion on the implementation of these library functions and why you may want to understand their efficiency (or, you simply may not care; as a student in first year, you'll likely not care, as a software developer with constraints regarding mainly space complexity, you may indeed care). Finally this set of notes on lecture six is summarised and references are provided.

#### 1. Administrative "Stuff" Within Lecture Six

> "If you see me on the street, say: WoW, I really like your classes! You're changing my life!" <br />
> — Dr Phillip Wadler, University of Edinburgh, Functional Programming Lecture Six [[1]](#1)

The above statement may have been made tongue-in-cheek. However, I do find it difficult to disambiguate communicative statements at times (for genuine reasons). Thus, I feel somewhat obliged to reference a statement previously made within <code>1aa5d726d3b4b3176520fb9ab5c474d0254fca4b</code>:

> "My thanks go out all individuals associated with IOHK who have provided such an amazing degree of encouragement. This technology and this community has changed my life." <br />
> — Note From The Author: My Sincerest Thanks

Whilst others may disagree (as we enter into the realm of armchair philosophy), I do believe it is important to recognise that no matter how insignificant one may seem to be, any decision made by one does indeed make a difference.

> "It makes a difference, first of all in material terms. It makes a difference to other people and it sets an example." <br />
> — Dr Robert C. Solomon, Professor of Philosophy at the University of Texas at Austin [[2]](#2)

Whilst I write the above in the spirit of conveying gratitude, it must also be recognised (in my humble opinion) that what one does insofar as shipping code is concerned also makes a difference. I won't journey too off-track here, but there should be a larger degree of responsibility placed on shipping correct code [[3]](#3), which is also touched upon within the lecture.

#### 2. The Whole Point Of Functional Programming

The crux of the lecture opens with Abstraction. It's interesting to note that abstraction as defined within lecture six is "recognising a pattern and giving it a name", thus a function is a form of abstraction. One may similarly define abstraction as "the elimination of the irrelevant and the amplification of the essential" [[4]](#4). I can't help but feel (given the second definition) that notions of what may be relevant and essential differ from person to person. Abstracting away too many features using syntactic sugar may make programs easier to read (to the educated individual) but somewhat more confusing to a student of the language. Thus, we're back to trying to evaluate the balance between clarity, conciseness and execution speed (which we spoke about during the previous lecture). The question I find myself asking is: how exactly do you strike that balance? As an example, using the shorthand operator <code><$></code> in place of <code>fmap</code> was somewhat confusing to me during the PPP lectures. Perhaps writing code as explicitly as possible during a demonstration or an example and then re-factoring it with a clear explanation as to why we're doing so would be advantageous (beneficial to 'greener' students). Again, my 2-cents.

#### 3. Some History on Mathematics, Computation, Language, Formal Systems and Logic

* Alan Turing - I think we all know about Alan Turing! Turing Machines, The Chruch-Turing Thesis, Halting Problem, etc.
* Gödel (Incompleteness Theorems [[5]](#5)).
* It may also be worth mentioning Hilbert, the following couple of minutes from Lex.F's podcast touches on these points in history (and is pretty interesting, although they quickly venture into the philosophy of language, which isn't necessarily a bad thing. It's probably a good idea for computer scientists to aquire a more wholesome view on some of these subjects such as Phil of Mind, Language, etc - in my humble opinion): [https://youtu.be/P-2P3MSZrBM?t=907](https://youtu.be/P-2P3MSZrBM?t=907).
* Church (The Relevance of the Church-Turing Thesis [[6]](#6) - I was surprised more people had heard of Gödel than Alonzo Church).

#### 4. Touching on List Comprehensions and Recursive Functions

Consider the following:

```haskell
Prelude> :{
Prelude| sqs :: [Int] -> [Int]
Prelude| sqs xs = [ x*x | x <- xs ]
Prelude| :}
Prelude> sqs [1,2,3,4]
[1,4,9,16]
Prelude> -- squares each value in the list and can be read as:
Prelude> -- given a list of Ints sqs returns a list of Ints
Prelude> -- such that the returned list contains elements
Prelude> -- x*x given x is drawn from the list xs (formal param)
Prelude> -- I think of this as an iterative process, but
Prelude> -- it's neat, it's concise, it's much nicer than a loop
Prelude> -- alternatively:
Prelude> :{
Prelude| sqsRec :: [Int] -> [Int]
Prelude| sqsRec []          =   []
Prelude| sqsRec (x:xs)      =   x * x : sqsRec xs
Prelude| :}
Prelude> sqsRec [1,2,3,4]
[1,4,9,16]
Prelude> (sqs [1,2,3,4]) == (sqsRec [1,2,3,4])
True
```

*Conventional Wisdom: instead of simply running a single equivalence check,* **you should** *run multiple checks using QuickCheck (or a similar testing framework).*

#### 5. The General Case & The Map Function

*I stopped the lecture here and implemented what my intuition tells me about the mapping function: map.*

*I was using a GHCi shell that was previous open where I was playing around with Functors, hence the replacement of prelude> with Functors>*

```haskell
*Functors> xss = [1,2,3,4]
*Functors> :{
*Functors| sqs :: [Int] -> [Int]
*Functors| sqs []          =   []
*Functors| sqs (x:xs)      =   x * x : sqs xs
*Functors| :}
*Functors> sqs [1,2]
[1,4]
*Functors> map sqs [1,2]

<interactive>:22:10: error:
    • No instance for (Num [Int]) arising from the literal ‘1’
    • In the expression: 1
      In the second argument of ‘map’, namely ‘[1, 2]’
      In the expression: map sqs [1, 2]
*Functors> :{
*Functors| pd :: Int -> Int
*Functors| pd x = x * x
*Functors| :}
*Functors> pd 4
16
*Functors> map pd [1,2,3]
[1,4,9]
*Functors> map sqs [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,9],[16,25,36],[49,64,81]]
*Functors>
```
Right, so you can see where my intuition led me slightly astray, because sqs returns a list of Int, I cannot map sqs to a list, rather, it must be (as I realised) a list of lists. However, if I simply define pd (albeit, a misleading name -- sorry!), as a squaring function which returns a single int, then sure, I can map that function over a list. *So, this is my second time trying to learn Haskell, it's beginning to click; and it feels good.*

Now I imagine when I resume this video, we're going to see a more general case, insofar as the use of say Num is concerned. I would imagine, typically, if you're going to map over a list, set, or whatever of values, you'll likely want to use type inference and let the compiler do some of the work for you. Although, I would assume that there may also be cases where you REALLY want to specify explicit types. Again, how do we strike that balance of clarity ~ abstraction?

*Unpause.*

Okay, cool. It looks like my intuition was fairly spot on, except for the fact that I'm declaring functions within a more global scope rather than using helper functions. Then I'm simply testing the implementation of 'squares' using an explicit call rather than defining a function containing map and calling that function. So long as I understand this, I see no issue insofar as reimplementing the same process. I know we've been over it, but how much actual utility is there in the use of helper functions over defining each function separately? Readability considerations, memory use considerations (helper functions are out of scope as soon as a new function with the same name as it's 'parent' is defined, thus potentially reducing the amount of memory allocation required for the execution of your program?) - how many of these are valid concerns?

#### 6. "the people who invented C were ignorant... They were not educated in these ideas."

*You can skip this section, it's just a bunch of rambling really!*

Right, so I can't really comment on this, but it's never stopped me in the past. I cannot help but feel that there is somewhat of a biased opinion here: C is bad? Haskell is good? Which is, as John Searle would phrase it, epistemically subjective [[7]](#7). Note: it's also worth reviewing this reference [[7]](#7) if you're interested in Philosophy of Mind, Philosophy of Language and Artificial Intelligence). *Again, this may have been a tongue-in-cheek statement?* However, I would always posit that for any task that involves engineering of software, there is an appropriate tool for the job. *With this said, I would like to add that entering into programming with a mathematical framed mind is likely advantageous, as I have heard various people who I look up to echo this sentiment.*

In summary: we can all have a bit of a poke at one another for a bit of fun, but ultimately: given a software engineering task there are multiple tools available to us; and there will always be a subset of tools which provide optimal implementations given the constraints of the task. I do think this is important to keep in mind insofar as it's important to keep somewhat of an open mind.

With that said, the lecturer is in fact correct. If you review Simon Peyton Jones' video on Useful and Useless languages (which is a funny watch), there is more and more cross-fertilisation going on. For example, I remember using LINQ for my undergraduate project during third year. LINQ in C# draws elements from functional programming.

*Edit: I guess C is somewhat tolerable, or a necessary evil after all! [[8]](#8)*

*End of opinionated monologue! It was quickly written out, I apologise for any spelling or grammatical mistakes!*

#### 7. Filter Pattern

Okay, so this is straight forward. You're essentially adding a condition to a list comprehension or a recursive call through the use of a guard (the predicate holds true given the current element in the list or recursive call). I've already kind of done this without knowing when counting upwards from 0 to 100, by 3:

```haskell
*Functors> :{
*Functors| abc :: Int -> Int -> Int -> [Int]
*Functors| abc fr to by = [x | x <- [fr..to], x `mod` by == 0]
*Functors| :}
*Functors> abc 0 100 3
[0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
*Functors> -- this is the same as:
*Functors> filter ((== 0) . (`mod` 3)) [0..100]
[0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
*Functors> -- I can't help but feel this is somewhat unreadable...
*Functors> :{
*Functors| cMod :: Int -> Int -> Bool
*Functors| cMod x y = x `mod` y == 0
*Functors| :}
*Functors> cMod 10 3
False
*Functors> cMod 9 3
True
*Functors> -- slight problem, predicate function takes two arguments..
*Functors> :{
*Functors| cMod3 :: Int -> Bool
*Functors| cMod3 x = x `mod` 3 == 0
*Functors| :}
*Functors> cMod3 9
True
*Functors> cMod3 10
False
*Functors> filter cMod3 [0..100]
[0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99]
*Functors> -- I really REALLY like list comprehensions, I need to start doing more recursion..
*Functors> -- But List Comprehensions make me happy :'(
```

### 8. SERIOUS QUESTION

#### Are these patterns implemented as List Comprehensions or via Recursion as library functions within the prelude? The question is: why should we care?

*Okay, so I've paused the video once again. Here is my intuition.*

We should care as to how they're implemented mainly due to calculating the amount of memory required during execution. I imagine we would like to be able to calculate time complexity and space complexity such that we can provide the required resources to actually execute our code?

So long as the values being provided to these library functions isn't arbitrary IO, we should be able to calculate time complexity and space complexity given both implementations. Thus, the question is: which is more efficient? My assumption would be library functions are going to favour efficiency over clarity, because they've been formally proven to function as expected (at least I hope they have). It wouldn't make much sense to implement library functions in the core prelude for clarity rather than efficiency. That's my intuition.

*Unpause.*

Okay! I was actually really happy with my intuition here, even though it was stated that we shouldn't care **with the exception** that I outlined above. Sometimes we do want to calculate the efficiency of our programs. This is especially apparent when you look at the recent teething problems that were encountered during the Alonzo hard fork on Cardano. Whilst I assume the library functions are efficient, we still need to know what the efficiency bounds are in order to write code given specific constraints.

**Happy about this :)**

### 9. Fold *(Deadly Silence...)*

*Oh lord, I finally understand the function signatures. It's taken me a while. Example below:*

```haskell
*Functors> -- foldr accepts a function with the signature (a -> a -> a)
*Functors> -- it also accepts two more arguments, a and [a]
*Functors> -- further, it returns a
*Functors> -- the part I found confusing with these library functions
*Functors> -- was the initial argument: (a -> a -> a), but it's just the
*Functors> -- return value from another function, thus allowing a function
*Functors> -- with that signature to be passed as an argument. Silly me!
*Functors> -- I am learning this by myself, and it is taking time to click
*Functors> -- but I can feel it starting to click :)
*Functors> :{
*Functors| foldrr :: (a -> a -> a) -> a -> [a] -> a
*Functors| foldrr f v []           =   v
*Functors| foldrr f v (x:xs)       =   f x (foldrr f v xs)
*Functors| :}
*Functors> :{
*Functors| summ :: [Int] -> Int
*Functors| summ xs = foldrr (+) 0 xs
*Functors| :}
*Functors> summ [1..100]
5050
*Functors> summ [1..10]
55
*Functors> :t (+)
(+) :: Num a => a -> a -> a
*Functors> -- makes sense, right?
```

*Not too much more goes on...*

### 10. Summary

We covered quite a bit here! Some interesting history (I encourage you to check out the references if you feel so inclined). Most importantly, we cover the library functions Map, Filter and Fold. During the exploration of this space some fairly *serious* questions come up (within the lecture), we comment on these within the notes. Any feedback is very much welcome. I look forward to continuing these notes and my journey through the second attempt to learn Haskell! Well, it's my third attempt if you consider my time at university. It would appear most people are in agreement that it takes a few attempts (due to our jaded nature insofar as us imperative programmers are concerned). All in all, I'm happy. Don't worry, be happy :)

### 11. References

<a href="#1" id="1">1</a>. Wadler, P., 2020. <br />
Lecture Six, Functional Programming. <br />
The University of Edinburgh, Informatics UG. <br />
Available at: <https://media.ed.ac.uk/playlist/dedicated/179956591/1_280z8cps/1_k00my6ho> <br />
Last accessed: 17th October, 2021.

<a href="#2" id="2">2</a>. Solomon. R.C. <br />
Professor of Philosophy at the University of Texas at Austin. <br />
Extract from Waking Life <br />
Available at: <https://youtu.be/WVZjuidPmus?t=96> <br />
Last accessed: 17th October, 2021.

<a href="#3" id="3">3</a>. Martin, R.C., 2011. <br />
The clean coder: a code of conduct for professional programmers. <br />
Pearson Education.

<a href="#4" id="4">4</a>. Martin, R.C., Newkirk, J. and Koss, R.S., 2003. <br />
Agile software development: principles, patterns, and practices (Vol. 2). <br />
Upper Saddle River, NJ: Prentice Hall.

<a href="#5" id="5">5</a>. Author @ Standard. <br />
Available at: <https://plato.stanford.edu/entries/goedel-incompleteness/#RelChuTurThe> <br />
Last accessed: 17th October, 2021.

<a href="#6" id="6">6</a>. Author @ Standard. <br />
Available at: <https://plato.stanford.edu/entries/goedel-incompleteness/> <br />
Last accessed: 17th October, 2021.

<a href="#7" id="7">7</a>. Searle, John R., 2015. <br />
Consciousness in Artificial Intelligence. <br />
Talks At Google. <br />
Available at: <https://youtu.be/rHKwIYsPXLg?t=98> <br />
Last accessed: 17th October, 2021.

<a href="#8" id="8">8</a>. Simon L. Peyton Jones and Philip Wadler. 1993. <br />
Imperative functional programming. <br />
In Proceedings of the 20th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '93).<br />
Association for Computing Machinery, New York, NY, USA, 71–84.
