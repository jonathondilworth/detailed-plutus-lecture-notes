module Main where
import Test.QuickCheck

-- Note; not 100% sure what is being asked, surly the compiler has to follow
-- tokenisation (lexical analysis) -> syntactical analysis -> ..?
-- There is an order, unless you specify, say: (double 2) + double 2, right?
-- ... That would be (2 + 2) + double 2, expand out..
-- Is this more of a thought experiment?
-- How about this:
-- double (double 2)
-- -- now we have two INDPENDENT CALCULATIONS introduce parallelism to compiler
-- double 2 + double 2
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8

-- Summation
summ :: Num a => [a] -> a
summ [] = 0
summ (x:xs) = x + summ xs

-- if we multiplied by zero initially, we'd get zero every time!
pd :: Num a => [a] -> a
pd [] = 1
pd (x:xs) = x * pd xs

-- reverse qsort
rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

main :: IO ()
main = do
  -- not exactly proving for any number.. How exactly would you write this proof?
  putStrLn $ show (summ [1] == sum [1])

  -- product
  putStrLn $ show (pd [2,3,4])

  -- reversing qsort: fairly sure you can swap the larger and smaller around..
  putStrLn $ show (rqsort [3,5,1,4,2])

  -- replacing <= with < allows any duplicate element is never < or >, and is
  -- thus removed from the final sorted list. This could be a feature, not a
  -- a bug depending on the design of your program.