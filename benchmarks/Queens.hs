import Prelude hiding ( concatMap, length, and)

and False a = False
and True  a = a

append []     ys = ys
append (x:xs) ys = x : (append xs ys)

concatMap f []     = []
concatMap f (x:xs) = append (f x) (concatMap f xs)

length []     = 0
length (x:xs) = (+) 1 (length xs)

gen nq n =
  case (==) n 0 of
    True  -> []:[]
    False -> concatMap (gen1 nq) (gen nq ((-) n 1))

gen1 nq b = concatMap (gen2 b) (toOne nq)

gen2 b q = case safe q 1 b of
             True  -> (q:b) : []
             False -> []

safe x d [] = True
safe x d (q:l) =
  and ((/=) x q) (
  and ((/=) x ((+) q d)) (
  and ((/=) x ((-) q d)) (
  safe x ((+) d 1) l)))

toOne n = case (==) n 1 of
            True  -> 1:[]
            False -> n : toOne ((-) n 1)

nsoln nq = length (gen nq nq)

main = print $ nsoln 10
