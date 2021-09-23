# Preface ++ General Notes

<small><code>| (++) :: [a] -> [a] -> [a]</code></small>

<small>*So I've temporarily ran into a roadblock, I'm waiting on a patch panel and some networking equipment to arrive before I can finish my home office. Time to put down the drill, I'm a computer scientist, well at least I tell myself that, I probably shouldn't be handling drills anyway...*</small>

**Note: If you don't enjoy rambling, perhaps you should skip straight to the lectures notes!**

<hr />

Perhaps you noticed? I'm a tad interested in Blockchains and Distributed Ledgers (and have been for a while now). However, I was somewhat disheartened by the sheer number of individuals willing to [take advantage of others](https://youtu.be/Y6vHVINBi0s) <sup><a href="#1">1</a></sup> during the early days, it drove me away (rather unfortunately, otherwise I would be on a yacht right now, I am only kidding, **life is not ALL about money, you do need enough, but please don't waste your time doing something that makes you unhappy**). On that somewhat inspiring note, let's got into some exciting stuff! Allow me to explain what I'm doing here.

### What's Going On?

So, if you're like me, you haven't written too much Haskell before *(until the Plutus Pioneer Program)*. Sure, I wrote a few lines during my undergraduate degree. I messed about with Scala as well, which is by no means as purely functional as some other languages. When it came to my PG, I had to do **some** Haskell (which tends to be written in a fairly **declarative** manner, however for connivance, we mostly used Python *- ML, NLP, it was a PG in AI*). Now, if you're new to all this, perhaps you're scratching your head. **Don't worry!** These notes are here for a reason, and if you're reading them, that's great! It's always nice to be able to offer a degree of help and/or mentorship. Hopefully you'll come out the other side knowing more than you did previously, but as is the case with every 'course' or set of lecture notes, **success is parameterised by YOU!** You may have heard of the saying:

> "Garbage in! Garbage out!" <br />
> — George Fuechsel [[1]](#1)

*(we'll come back to this because it has some implications elsewhere)*

**Back to the point at hand.** Ahh yes, declarative languages. Syntactically, declarative languages express a solution to a computation in the form of a set of pure functions. What is a set of pure functions? Exactly what it sounds like. There are some well defined constraints as to what makes a language declarative, that is to say: completely pure (sounds almost angelic doesn't it?), these can be read about in more depth [here](). On the other hand, you can listen to Simon Peyton Jones talk about how useless Haskell is [here](https://youtu.be/iSmkqocn0oQ) *(tongue-in-cheek)*. In short: IO is bad if (**and only if**) the program does not return the same result (is non-deterministic) such that *(.s.t.)* the same IO is provided on every iteration of its execution. That's another phrase you'll hear these logicians throw around by the way! **If and only ****ing if**, I wondered what that meant for about six months as an undergraduate (why would you repeat yourself? I was told: [**Don't Repeat Yourself**](https://journals.plos.org/plosbiology/article/info:doi/10.1371/journal.pbio.1001745) [[2]](#2), I'm telling you, these mathematicians, they're either **extremely cleaver and lazy** (as it turns out Haskell is pretty lazy) or a bit loopy! So let's be completely clear: it implies the following bidirectional property: the RHS must equal the LHS given a proposition involving (typically) two statements with a connective between them. Yes, I know, this is all rather abstract for your average Joe *(and you're probably asking yourself: why do I need to know this, just like you did when you learnt about Pythagoras' theorem, but that came in handy didn't it!?)*. Let me put it this way: **I am exactly twenty years old** *if and only if* **I have lived for exactly twenty years** - you can read more about (the short hand) iff [here](https://math.stackexchange.com/questions/68293/what-is-the-difference-between-only-if-and-iff))... In mathematics I believe it's called **equivalence**... Wowzers, talk about tangents! *(What was that about Pythagoras?)*

*(Don't worry, I will arrive at a discernible point soon, honest. I'm hoping this will all come together beautifully! If it doesn't I'll edit and re-edit - and hopefully get some feedback? I'm writing creatively because I'm fairly certain it's helpful to minimum some people, this is mentioned in the parent directory README.md)*

Is this is suppose to be an authors note, or an introduction to my stream of consciousness... I was talking about language <sup><a href="#2">2</a></sup> wasn't I? Right, I was? Fantastic! So, providing a computer program *(which can be modelled as an automaton, which we'll come back to later!)* with IO is *OKAY* if the program is **very simple** and **deterministic**. Meaning it can take some data, perform a computation of any degree of complexity (which falls within the realms of discrete mathematical systems - maybe don't allow that on-chain though!), but it **will always return the same result!** This is important, which is why I've written it twice, because it's a principal known as **referential transparency**.

Now, perhaps you should take a minute. Is your head spinning? No? Great! Now, think about it... If you construct very small... I don't want to use the word block here (component may be appropriate), but let's go with an analogy and use the word brick! If you were to create a large set of bricks, and you appropriately laid them (and you included a very secure door), you get the **emergence** of a **structure**, as an undergraduate perhaps it would be a shed? (You would be surprised how many sheds you find in industry, so **don't worry!**).

Now, let's get to it. The design of these structures (which we all know are programs, that's what I'm actually talking about here) is **extremely important!** Why? Because the design comes before the implementation. If you fail the design, you will likely fail the implementation (unless you get really lucky, in which case, you're actually unlucky - trust me). When we build houses, we don't build them to fall over. Similarly, when we build aeroplanes, we don't build them to restrict the pilot insofar as a '*catastrophic failure*' occurs, which may mean the loss of life. With regards to our industry (if you're reading this as a primer for the PPP), if you fail at design, the first domino falls, and you fail at implementation. No biggie right? If at first you don't succeed, try, try again? **Nope!** These systems have other people (who you should give some consideration to) and their assets tied to them. In some instances, yes, those assets are lives. Whilst I'm if at least average intelligence (I hope), I know you will always get bugs in code. Regardless, it does not excuse failing to strive for perfection (if, and only if: other people besides yourself are effected, you should be free to do as you please).

### Right, Ethics: Covered! Next, Language 2.0:

You've now heard of declarative languages (purely functional languages have specific constraints, Remember, you can write any language functionally, even those horrible ones!). You may have already heard of imperative languages, these can either be procedural (do this, now this, now this - a script), or they can be structured in some way. Perhaps they use design principals such as object orientation, or perhaps they're completely modular. Either way, we're not going to be talking much about them. All you need to know is that you typically have two types of language when it comes to talking to computers (which I do on a regular basis). You can either use a Declarative protocol, or you can use an imperative one. Declarative languages are typically functional (but you can get some languages which are not purely functional - google is your friend!). Finally, imperative languages are either procedural (like ARM or X86, or even a python script, but that has an interpreter sitting between it... Well, I guess assemblers are a thing too... Not quite the same though are they); ANYWAY. Alternative imperative languages are either procedural, modular or OOP based languages like Java, C# (with the exception of LINQ, which came in really handy for me. For some reason, my undergraduate supervisor wanted me to implement a feedforward neural network and back-propagation using C# - and he was obsessed with F#, lovely guy, but.. WHY? Plus, Windows - eww). He did his very best to help me though and that, I believe, matters more than anything else. Yes, research is important, but IMO there is no room for selfishness and competitiveness in an educational setting. I guess that's why I like it so much here (The Cardano Community / IO(G)).

Right, I think that's it for my little authors note, which turned out to be.. kind of a lecture in and of itself? Anyway, if you made it this far, I hope you learnt something and enjoyed the read! Sometimes it can be difficult to find the motivation to continue with all this. Functional programming is fairly new to me too! So any feedback is welcomed beyond belief. 

**I shall leave you with this, which is one of my favourite quotes.**

> "It might be true that there are six billion people in the world and counting. Nevertheless, what you do makes a difference. It makes a difference, first of all, in material terms. It makes a difference to other people and it sets an example." <br />
> ― Robert Solomon, Waking Life

*Think about that. Why are we here? Perhaps it's subjective, but for me, it's to make a difference. A positive one. We can all start by setting an example (and trying our best). So, now that you're all highly encouraged, let's learn us a Haskell!* **FOR GREAT GOOD!** *(That's right, finally: the discernible point).*

#### References

<a href="#1" id="1">1</a>. Raymond, A.H., Young, E.A.S. and Shackelford, S.J., 2017. <br />
Building a Better HAL 9000: Algorithms, the Market, and the Need to Prevent the Engraining of Bias. <br />
NW. J. Tech. & Intell. Prop., 15, p.215.

<a href="#2" id="2">2</a>. Wilson, G., Aruliah, D.A., Brown, C.T., Hong, N.P.C., Davis, M., Guy, R.T., Haddock, S.H., Huff, K.D., Mitchell, I.M., Plumbley, M.D. and Waugh, B., 2014. <br />
Best practices for scientific computing. <br />
PLoS biology, 12(1), p.e1001745.


#### Endnotes

<a href="#1" id="1">1</a>. I know what it feels like when an exchange disappears, see you later BritCoin/Intersango! Goodbye Money!

<a href="#2" id="2">2</a>. Perhaps you would enjoy Wittgensteins' The Tractatus.