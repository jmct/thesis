import Prelude hiding ( foldr1, map, length
                      , zip, filter, flip, and
                      )

data Pair a b = P a b
data Prop     = And Prop Prop
              | Const Bool
              | Implies Prop Prop
              | Not Prop
              | Var Char

find key ((P k v):t) = case (==) key k of
                               True  -> v 
                               False -> find key t 

eval s (Const b)       = b 
eval s (Var x)         = find x s 
eval s (Not p)         = case eval s p of
                         True  -> False 
                         False -> True 
eval s (And p q)       = case eval s p of
                         True  -> eval s q 
                         False -> False 
eval s (Implies p q)   = case eval s p of
                         True  -> eval s q 
                         False -> True 

vars (Const b)     = [] 
vars (Var x)       = [x] 
vars (Not p)       = vars p 
vars (And p q)     = append (vars p) (vars q) 
vars (Implies p q) = append (vars p) (vars q) 

bools n = case (==) n 0 of
          True  -> []:[] 
          False -> let bss = bools ((-) n 1) in
                   append (map (False:) bss)
                          (map (True:)  bss) 

neq x y = (/=) x y

rmdups []     = [] 
rmdups (x:xs) = x:rmdups (filter (neq x) xs)

substs p = let vs = rmdups (vars p) in
           map (zip vs) (bools (length vs)) 

isTaut p = and (map (flip eval p) (substs p)) 

flip f y x = f x y 

length []     = 0 
length (x:xs) = (+) 1 (length xs) 

append []     ys = ys 
append (x:xs) ys = x:append xs ys

map f []     = [] 
map f (x:xs) = f x : map f xs

and []     = True 
and (b:bs) = case b of
                  True  -> and bs 
                  False -> False 

filter p []     = [] 
filter p (x:xs) = case p x of
                       True  -> x:filter p xs
                       False -> filter p xs 

null []     = True 
null (x:xs) = False

zip []     ys     = [] 
zip (x:xs) []     = []
zip (x:xs) (y:ys) = (P x y) : (zip xs ys) 


foldr1 f (x:xs) = case null xs of
                       True  -> x 
                       False -> f x (foldr1 f xs) 

imp v = Implies (Var 'p') (Var v) 

names = "abcdefghijklmn" 

testProp = Implies
             (foldr1 And (map imp names))
             (Implies (Var 'p') (foldr1 And (map Var names))) 

main = print $ case isTaut testProp of
       True  -> 1 
       False -> 0 
