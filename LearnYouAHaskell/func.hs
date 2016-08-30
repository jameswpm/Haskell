imcTell :: (RealFloat a) => a -> String
imcTell imc
  | imc <= 18.5 = "You're underweight, you emo, you!"
  | imc <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | imc <= 30.0 = "You're fat! Lose some weight, fatty"
  | otherwise = "You're whale, congratulations"

imcTell' :: (RealFloat a) => a -> a -> String
imcTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty"
  | otherwise = "You're whale, congratulations"

imcTell2 :: (RealFloat a) => a -> a -> String
imcTell2 weight height
  | imc <= 18.5 = "You're underweight, you emo, you!"
  | imc <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | imc <= 30.0 = "You're fat! Lose some weight, fatty"
  | otherwise = "You're whale, congratulations"
  where imc = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat)  = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

--Function using where
calcBmis :: (RealFloat a) =>  [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

-- Same function using let
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let
    sideArea = 2 * pi * r * h
    topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- Pattern Matching using case expressions
head1' :: [a] -> a
head1' xs = case xs of [] -> error "No head for empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
   where what [] = "empty."
         what [x] = "a singleton list."
         what xs = "a longer list."
