import Prelude hiding ( foldr, foldl, map, concatMap, sum, tail
                      , null, length, transpose, reverse, zipWith
                      , flip, and
                      )

and False a = False
and True  a = a

map f []     = []
map f (x:xs) = f x : map f xs

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
