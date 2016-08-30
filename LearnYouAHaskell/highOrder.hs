-- test some partial applied function
multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

dividByTen :: (Floating a) => a -> a
dividByTen = (/10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

--quicksort implementation using filter
quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = [];
quickSort' (x:xs) =
  let smallerSorted = quickSort' (filter (<=x) xs)
      biggerSorted = quickSort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

-- Find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- Find the sum of all odd squares that are smaller than 10,000
findSumOfOdds = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
findSumOfOdds' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Exercise in Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain ((n * 3) +1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- numLongChains using lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Equivalent functions. Understanding curried functions
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- Definig Flip  function using lambda
flip1' :: (a -> b -> c) -> b -> a -> c
flip1' f = \x y -> f y x

-- Implementing sum and elem using foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
