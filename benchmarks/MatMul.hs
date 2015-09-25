import Prelude hiding ( foldr, foldl, foldr1, map, concatMap
                      , sum, tail, null, length, transpose
                      , reverse, zipWith, zip, filter, const
                      , replicate, flip, and
                      )

null []      =  True
null (x:xs)  =  False

matMul xss yss  =  map (mulRow (transpose yss)) xss

mulRow yssTrans xs  =  map (dotProduct xs) yssTrans

dotProduct xs ys  =  sum (zipWith (*) xs ys)

transpose (r:rs) =  case null rs of
                         True -> map (:[]) r
                         False -> zipWith (:) r (transpose rs)

zipWith f []         []          =  []
zipWith f (x:xs) (y:ys)  =  f x y : zipWith f xs ys

sum xs  =  foldr (+) 0 xs

foldr f z []     =  z
foldr f z (x:xs)  =  f x (foldr f z xs)

map f []          =  []
map f (x:xs)  =  f x : map f xs

onesMat n  =  replicate n (replicate n 1)

replicate n x  =  case ((==) n 0) of
                  True  -> []
                  False -> x:(replicate ((-) n 1) x)

main  = print $ matMul (onesMat 50) (onesMat 50)
