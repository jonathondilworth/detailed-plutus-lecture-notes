module Main where

-- Example From: Graham Hutton's Functional Programming In Haskell
-- implementation of QuickSort


-- we're not declaratively using (lists of) Integers here, compiler infers type? Lazy.
-- However, it does mean any 'orderable' type can be 'QuickSort'ed! Lovely!
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

main :: IO ()
main = do
   let result = qsort [3,5,1,4,2]
   -- putStrLn $ show result
   print result

   -- What's Happening?
   -- qsort is taking 'x' (the first index within the list)
   -- qsort then draws all values from xs which are <= x and drops them in a list
   -- qsort also draws all values from xs which are > x and drops them in a list
   -- qsort then appends the lists together (++)
   -- since qsort calls itself, it will recursively sort the lists into individual items (OF LISTS!)
   -- thus, ordering them:
   -- ([] ++ [1] + [2]) ++ [3] ++ ([4] ++ [5] ++ [])
   -- [1,2] ++ [3] ++ [4,5]
   -- [1,2,3,4,5]