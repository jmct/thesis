import Prelude hiding ( foldr, foldl, map, concatMap, sum, tail
                      , null, length, transpose, reverse, zipWith
                      , const, replicate, flip, and
                      )

data Shrub   = Root Node
data Node    = Void | Fork Shrub Int Shrub

data Test a  = A a | B a a | C


tail (x : xs) = xs

const a b = a

one p []       = []
one p (x : xs) = const (case p x of
                             True -> x : []
                             False -> one p xs) 0

map f []     = []
map f (x:xs) = f x : map f xs

append []     ys = ys
append (x:xs) ys = x : (append xs ys)

concatMap f []     = []
concatMap f (x:xs) = append (f x) (concatMap f xs)

length []     = 0
length (x:xs) = (+) 1 (length xs)

replicate n x =
  case (==) n 0 of
    True  -> []
    False -> x : replicate ((-) n 1) x

l = 0
r = 1
d = 2

eq x y = (==) x y

left  xs = map (one (eq l)) (tail xs)
right xs = [] : map (one (eq r)) xs
down  xs = map (one (eq d)) xs

merge []     ys     = []
merge (x:xs) []     = x : xs
merge (x:xs) (y:ys) = append x y : merge xs ys

next mask = merge (merge (down mask) (left mask)) (right mask)

fill []     = []
fill (x:xs) = append (lrd x xs) (map (x:) (fill xs))

lrd []     ys = [[l,r,d]:ys]
lrd (x:xs) ys = []

solve n mask =
  case (==) n 0 of
    True  -> []:[]
    False -> concatMap (sol ((-) n 1)) (fill mask)

foo n =
  case (<=) n 5 of
    True  -> B True False
    False -> C

sol n row = map (row:) (solve n (next row))

nqueens n = length (solve n (replicate n []))

main = print $ nqueens 10
