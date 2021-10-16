#### Quick (and sloppy, I guess?) Introduction to map, fmap and Functors

*I'm sort of in flow here -- struggling with difficult problems and then overcoming them is quite a good way of learning a new 'paradigm' of programming.*

Okay, so quickly... <code>map</code> has a f-signature: <code>map :: (a -> b) -> [a] -> [b]</code> where you provide a function (even though it doesn't really look like that from the signature IMHO) and a list of elements. Then you're going to **map** that function to every element within the list, such that:

```haskell
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
Prelude> :{
Prelude| crazyF :: Int -> Int
Prelude| crazyF x = x * (2 + 4 * 7) -- absolutely no reason, don't ask
Prelude| :}
Prelude> crazyF 15
450
Prelude> -- cool!
Prelude> -- now we can map our CRAYZ FUNCTION to a list of values of type Int
Prelude> map crazyF [10,250,45,2]
[300,7500,1350,60]
Prelude> -- cool, so how about fmap?
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> -- holy, a Functor!? Sounds.. Funky.
Prelude> -- so, now we're mapping a function to another function,
Prelude> -- given that the arguments to the inner function result in a value
Prelude> -- that can be accepted by the outer function? I think, I may need
Prelude> -- to brush up on this..
Prelude> -- so we can do stuff like:
Prelude> fmap crazyF (tail [10, 40, 60, 42, 420, 4200])
[1200,1800,1260,12600,126000]
Prelude> -- we can also use the shorthand <$>
Prelude> crazyF <$> tail [10, 40, 60, 42, 420, 4200]
[1200,1800,1260,12600,126000]
Prelude> -- in essence, we have a function which is mapping over another function.. I think
Prelude> -- I'm really trying to get to grips with this stuff, anything wrong here, please correct me.
```
Right. So, when I forked Monday Morning Haskells repo earlier this evening, I was kind of struggling, but the more **actual programming** you do, the more this stuff starts to slowly (at least, it seems that way) fit together.

Here's an example of some code that I was struggling with, but then you open GHCi, play around with the various functions, do some additional reading; and whilst it's still not 100% (or anywhere close to that), at least I feel as though I am making some progress:

```haskell
module Functors where

import           Data.Maybe (mapMaybe)
import qualified Data.Map   as M

-- Motivating Examples!

-- Simple String conversion. It might fail, so it returns Maybe
tupleFromInputString :: String -> Maybe (String, String, Int)
tupleFromInputString input = if length stringComponents /= 3
  then Nothing
  else Just (stringComponents !! 0, stringComponents !! 1, age)
  where
    stringComponents = words input
    age = read (stringComponents !! 2) :: Int

-- An alternative to using a tuple (String, String, Int)
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  }

personFromTuple :: (String, String, Int) -> Person
personFromTuple (fName, lName, age) = Person fName lName age

-- Converting between the two formats
convertTuple :: Maybe (String, String, Int) -> Maybe Person
convertTuple Nothing = Nothing
convertTuple (Just t) = Just (personFromTuple t)

-- Could not use `convertTuple` with the results of this function!
-- Would have to write a new function of type [(String, String, Int)] -> [Person]
listFromInputString :: String -> [(String, String, Int)]
listFromInputString contents = mapMaybe tupleFromInputString (lines contents)

{- Functor Definitions:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor (Either a) where
	fmap _ (Left x) = Left x
	fmap f (Right y) = Right (f y)
-}

-- TODO: This function needs a type signature! (Complete)
--       Make it as general as possible!
convertTupleFunctor :: Functor a => a ([Char], [Char], Int) -> a Person
convertTupleFunctor = fmap personFromTuple

-- Making our own Functor

data GovDirectory a = GovDirectory {
  mayor :: a,
  interimMayor :: Maybe a,
  cabinet :: M.Map String a,
  councilMembers :: [a]
}

instance Functor GovDirectory where
  
  {-
  -- MASS COMMENTED OUT
  -- TODO: Write out this functor instance!
  -- Had a go, eventually got there, but had to check the answers and see how I was doing
  fmap f oldDirectory = GovDirectory {
    -- fields that require filling..
    -- mayor, interimMayor, cabinet, councilMembers.. Right...
    -- REMEBER, everything is a value (***king FP) and values ARE functions in a lot of instances..?
    -- for example, x could be f(x) = x (identity)
    mayor = f oldDirectory mayor,
    interimMayor = f oldDirectory interimMayor,
    -- I wish someone would have introduced me to functional programming when I was writing
    -- PHP programs to do my further pure mathematics homework. I bet I would have actually been alright
    -- but now I'm jaded with years of imperative programming.
    cabinet = fmap cabinet oldDirectory, -- surely that's too easy?
    -- so it's a list of .. functions, so it's function mapping > 1 function..?
    -- councilMembers = f fmap councilMembers oldDirectory -- surly we need brackets here....
    councilMembers = f (fmap (councilMembers oldDirectory))
    -- OKAY. So I kinda didn't do too badly (honestly, almost stabbing in the dark)
    -- I need to remember the ordering of arguments to functions
    -- I am also doing the same thing when I apply functions as arguments to functions...
    -- okay, let's just use the implemented solution.
  }
  -}
  
  fmap f oldDirectory = GovDirectory {
    mayor = f (mayor oldDirectory),
    -- interimMayor = fmap f (interimMayor oldDirectory),
    -- cabinet = fmap f (cabinet oldDirectory),
    -- councilMembers = fmap f (councilMembers oldDirectory)
    -- I need to go back and check exactly what the <$> does again
    -- I remember it coming up in the PPP lectures
    -- Right, infix synonym for fmap, oh the alternative looks way nicer, I'm going to use that.
    interimMayor = f <$> (interimMayor oldDirectory),
    cabinet = f <$> (cabinet oldDirectory),
    councilMembers = f <$> (councilMembers oldDirectory)
  }

oldDirectory :: GovDirectory (String, String, Int)
oldDirectory = GovDirectory
  ("John", "Doe", 46)
  Nothing
  (M.fromList 
    [ ("Treasurer", ("Timothy", "Houston", 51))
    , ("Historian", ("Bill", "Jefferson", 42))
    , ("Sheriff", ("Susan", "Harrison", 49))
    ])
  ([("Sharon", "Stevens", 38), ("Christine", "Washington", 47)])

-- TODO: How can we do this in general terms, since we have
--       a Functor instance? (complete)
newDirectory :: GovDirectory Person
newDirectory = personFromTuple <$> oldDirectory
```

Right, I've spent most of my day on this Haskell stuff. As lovingly frustrating as it is, it's time to context switch to something else.