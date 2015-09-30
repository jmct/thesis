import Prelude hiding ( foldr, map, sum,
                      , length, filter)

myGcd x y = if y == 0 
          then x 
          else if x > y 
               then myGcd (x - y) y
               else myGcd x (y - x)

relPrime x y = myGcd x y == 1

euler n = length $ filter (relPrime n) xs
  where xs = [1..n]

filter p []     = [] 
filter p (x:xs) = case p x of
                       True  -> x:filter p xs
                       False -> filter p xs 

length [] = 0
length (x:xs) = (+) 1 (length xs)

map f []     = [] 
map f (x:xs) = f x : map f xs

foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

sum xs  =  foldr plus 0 xs 

main = print $ sum $ map euler [1..1000]
