import Prelude hiding ( foldr
                      , foldl
                      , map
                      , sum
                      , tail
                      , null
                      , length
                      , transpose
                      , reverse
                      , zipWith
                      , flip
                      )

tail (x:xs)  =  xs 

null []          =  True 
null (x:xs)  =  False 

single x  =  [x] 

length [] = 0
length (x:xs) = (+) 1 (length xs)

main  =  print $ map gridCount hidden 

gridCount word  =
  let d = transpose grid 
  in let r = grid 
     in let dr = diagonals grid 
        in let ur = diagonals (reverse grid) 
           in let dirs = [r,d,dr,ur] 
              in let drow  = reverse word 
                 in (+) (sum (map (dirCount word) dirs))
                        (sum (map (dirCount drow) dirs)) 

sum xs  =  foldr plus 0 xs 

plus x y = (+) x y 

foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

map f []     = [] 
map f (x:xs) = f x : map f xs

transpose (r:rs) = case null rs of
                        True  -> map single r 
                        False -> zipWith (:) r (transpose rs) 

 
diagonals (r:rs) = case null rs of
                        True  -> map single r 
                        False -> zipInit r ([] : (diagonals rs)) 

reverse xs  =  foldl (flip (:)) [] xs 

foldl f a []          =  a 
foldl f a (x:xs)  =  foldl f (f a x) xs 

flip f x y  =  f y x 

zipWith f []         []          =  [] 
zipWith f (x:xs) (y:ys)  =  f x y : zipWith f xs ys

zipInit []         ys           =  ys 
zipInit (x:xs) (y:ys)  =  (x:y) : zipInit xs ys

dirCount xs yss  =  sum (map (rowCount xs) yss) 

rowCount xs ys  =  count (prefix xs) (suffixes ys) 

count p []  =  0 
count p (x:xs)  =
  let c  =  count p xs in
  case p x of
  True -> (+) 1 c 
  False -> c 

suffixes xs   =  case null xs of
                 True  -> [] 
                 False -> xs : suffixes (tail xs)

prefix []     ys      = True 
prefix (x:xs) []      = False 
prefix (x:xs) (y:ys)  = case ((==) x y) of
                             True  -> prefix xs ys 
                             False -> False 

grid =
  [ "YIOMRESKST"
  , "AEHYGEHEDW"
  , "ZFIACNITIA"
  , "NTOCOMVOOR"
  , "ERDLOCENSM"
  , "ZOURPSRNDA"
  , "OYASMOYEDL"
  , "RNDENLOAIT"
  , "FIWINTERRC"
  , "FEZEERFTFI"
  , "IIDTPHUBRL"
  , "CNOHSGEION"
  , "EGMOPSTASO"
  , "TGFFCISHTH"
  , "OTBCSSNOWI"
  ]

hidden =
  [ "COSY"
  , "SOFT"
  , "WINTER"
  , "SHIVER"
  , "FROZEN"
  , "SNOW"
  , "WARM"
  , "HEAT"
  , "COLD"
  , "FREEZE"
  , "FROST"
  , "ICE"
  ]
