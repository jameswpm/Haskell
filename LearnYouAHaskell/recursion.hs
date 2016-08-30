-- Maximum function in a recursive way
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Maximum function also in a recursive way, but using max
maximum1' :: (Ord a) => [a] -> a
maximum1' [] = error "maximum of empty list"
maximum1' [x] = x
maximum1' (x:xs) = max x (maximum' xs)

-- Thinking recursivelly. Functions to replicate a element n times, take n elements from a list, reverse a list and also repeat a element
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- My implementation of repeat is basically the same thing that replicate - :(
repeat' :: (Num a, Ord a) => a -> i -> [i]
repeat' n _
  | n <= 0 = []
repeat' n y = y : repeat' (n - 1) y

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs


-- Awesome Quicksort in Haskell
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = [];
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a<= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
