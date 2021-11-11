*Unfortunately lost a bunch of notes as I've been sick for the last few days and my Mac Mini very kindly overheated, so we've brushed through some of the previous lectures, and we've kind of just jumping into IO, Monads, and I'll look to include some further information on Functors, Monoids, etc later.*

**General Notes from map lecture [x | x <- [17..], x < 22]**

```
Prelude Control.Monad Data.Char> lecture x = "lecture " ++ (show x)
Prelude Control.Monad Data.Char> map lecture [x | x <- [17..21]]
["lecture 17","lecture 18","lecture 19","lecture 20","lecture 21"]
```

### Monads are ~~WARN~~ WARM FUZZY THINGS!

***The Mind Body Problem***

*Pause Video*

* I Can Doubt Almost Everything
* The Only Thing I Cannot Doubt Is That I Am Doubting
* If I am Doubting, I am 'Thinking'
* I think, Therefore I Am
* This is more of an argument for Solipsism -> Idealism?
* Perhaps I am getting confused

*Unpause Video*

* Oh yes, the pineal glad.
* Let's not get started on the woo-woo ("Pondering and Panhandling!").

*Pause Video*

* Assuming one ascribes to Dualism, I seem to remember a number of counter-arguments:
	* Breaking the laws of physics: energy conservation?
	* Something about breaking the second law of thermodynamics?
	* (I suppose it depends on how you define the mind?)
	* References: Phil of Mind, brains, consciousness and thinking machines, Chalmers, Dennett, Patrick Grim, Searle J.
	* Audibles lecture series on Phil of Mind: Brains, Consciousness and Thinking Machines is a great introduction to anybody interested in Philosophy of mind (and potentially strong AI by the way, it's worth checking out).

*Unpause Video*

*Time to move away from Philosophy, back to Haskellz. What the heck is a REPL anyway?*

* **READ**
* **EVAL**
* **PRINT**
* **LOOP**
* ***R***, ***E***, ***P***, ***L***
* "The REPL"
* 'An interpreter' ?

**Function-esque Platforms For Real World Applications, Added Some Of My Own:**

* AWS Lambda, Functions as a Service, FaaS, Serverless

### Type: IO

```haskell
putChar :: Char -> IO ()
(>>) :: IO () -> IO () -> IO ()
-- (>>) :: Monad m => m a -> m b -> m b (?)
-- assuming IO is a Monad, then:
-- (>>) :: Monad m => m a -> m b -> m b
-- can be written as
-- (>>) :: IO () -> IO () -> IO ()
-- ?
Prelude Control.Monad Data.Char> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

* () is just an empty tuple.

*Question: how does the compiler know it's an empty tuple, not necessarily a triple? e.g: (\_,\_), rather than (\_,\_,\_)?*

* If I remember correctly (which I doubt), from the Plutus Pioneer Lectures, () is of type unit, or is that \_?

> "Units (written ()) can be understood as tuples with zero components." <br />
> Source: <https://sodocumentation.net/haskell/topic/5342/tuples--pairs--triples>

**(>>)** Binary Operator: How to make small talk with a Binary Operator?

* Are you commutative?
* Are you Associative?
* What is your identity?
* Identity for IO? ```done``` (like return *nothing goes here* in an OOP language?)
* ```done :: IO ()```
* Would it be accurate to describe done as a function that accepts IO () but does nothing if it is ever executed? That is what the function signature looks like?

### Read A String

*Okay, so I may have jumped a few... steps? I'll explain.*

```haskell
Prelude Control.Monad Data.Char> import Control.Monad
Prelude Control.Monad Data.Char> import Data.Char
Prelude Control.Monad Data.Char> :{
Prelude Control.Monad Data.Char| main :: IO ()
Prelude Control.Monad Data.Char| main = putStrLn "ENTER an UPPer or Lower case STR?!" >> shout <$> getLine >>= putStrLn
Prelude Control.Monad Data.Char| shout = map toLower
Prelude Control.Monad Data.Char| :}
Prelude Control.Monad Data.Char> main
ENTER an UPPer or Lower case STR?!
HELLO world UPPER lower
hello world upper lower
Prelude Control.Monad Data.Char> 
```

* Okay, so we're learning primarily in the REPL for now, once all these basics are down, I'll create a project or two.
* I've done some more messing around and didn't actually need to import Control.Monad, assumed we would need that for IO.
* (>>) will only execute the RHS is LHS is executed, in this case, if main is called, ```shout <$> getLine``` is also called, and since shout is a mapping function (for all elems in list, map: Data.Char.toLower); since shout is a function, we can use fmap (functor, short hand notation <$>, I believe we can also use . (dot)? ... anyway....
* The line entered at console will be map to lower, then (>>=) output to console.

*In the lectures, we have a reserved word 'done' - which I can't seem to find much (or any) information on.*

Ahah, done is just a special case of return. I was thinking, why do we have all these funny words!? return seemed to be most intuitive.

*Eww, if then else, see above code snippet.*

This do notation with curly braces looks like those horrible languages we are warned so much about. I'm guessing this is because we've not covered Functors and... maybe we'll get to the above code.

> If you stumble @ syntax, everything else becomes harder. <br />
> paraphrasing S.P.J

**Examples of Monoids**

I had no idea they were this simple.

A monoid is a pair of an operator, where the operator has the value as identity and is associative:

* (+) and 0
* (*) and 1
* (||) and False
* (&&) and True
* (++) and []
* (>>) and done (return?)

Some really cool ideas on these naming conventions, I had no idea they stretched so far back insofar as classical philosophy is concerned. Super interesting.

> "God Is An Abstract Data Type" <br />
> Prof Phil Wadler

<hr />

**Right, so I'm going to finish up watching these lectures; only three or four more to go? Write up some notes. At which point it'll be a good idea to try and implement the 'challenge' - which I may publish the RESULT from, but not the code that generates any image. Then it's time to revisit ALL THE PPP LECTURES AND CODE AND HWS. I wish I didn't currently have a head ache.**