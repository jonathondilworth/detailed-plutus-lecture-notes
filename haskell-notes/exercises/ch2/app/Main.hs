module Main where

doub :: Num a => a -> a
doub x = x + x

quad :: Num a => a -> a
quad x = doub (doub x)

adl :: Int -> [Int] -> Int
adl x xs = (x) `div` (length xs)

main :: IO ()
main = do
        -- define the list ONCE
        let sList = [1,2,3,4,5]

        -- let's make displaying stuff easy! Guess this is: print
        -- but what is life, if not but an exercise in absurdity
        let disp x = putStrLn $ show (x)
        
        -- playing around with some different ways could implement this..
        -- I don't like repeating myself, so I thought, what if we had a list
        -- of functions? Then you could draw from that list and apply sList
        -- as an argument to each function; however, some result in a list
        -- themselves... 
        let funcs = [head, sum, product]
        
        disp ([x sList | x <- funcs])

        -- Okay.. How else can we do this?
        -- I refuse to write things out one at a time!

        -- mFuncs :: [a] -> [[a]]
        -- This causes some problems, I wanted to express:
        -- given a list of functions, return a list of lists..

        let mFuncs fs = [x sList | x <- fs]

        let moreFs = [tail, (take 3), (drop 3), reverse]

        disp (mFuncs moreFs)

        {-
          right, so we've got head, tail, sum, take, drop, product and reverse
          yes, they do come in the form of lists... But, who doesn't love
          list comprehensions?
        -}

        disp (sList !! 2)
        disp (sList ++ sList)

        -- I imagine there is some Haskell genuius looking at this code and...
        -- shuddering. We've all got to start somewhere right, is this FP?

        -- 2.5:

        -- see functions defined @ top...

        {- let's import this into GHCi..?
           :l app/Main.hs
           *Main> quad 10
           40 
        -}

        -- WINNING!

        disp((adl 10 sList))

        -- Yep.

        {- something like this... Implemented it as a function above ^^^
        :{
          do
            disp((y) `div` (length ys))
          where
               y = 10
               ys = [1,2,3,4,5]
        
        :}
        -}

        -- 4. last selects last elem of list, implement it differently:

        disp((head (reverse sList)))

        -- 5. init removes the last element from a non-empty list. Deifne init two other ways!

        disp(reverse(drop 1 (reverse sList)))

        disp((take ((length sList) - 1)) sList)

        -- :)