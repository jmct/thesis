\begin{verbatim}
gcd x y = if y == 0
          then x
          else if x > y
               then gcd (x - y) y
               else gcd x (y - x)

fromto x y = if x > y
             then []
             else x : fromto (x + 1) y

map f []     = []
map f (x:xs) = f x : map f xs

relPrime x y = gcd x y == 1

filter p []     = []
filter p (x:xs) = if p x
                  then x : filter p xs
                  else filter p xs


length []     = 0
length (x:xs) = 1 + (length xs)

sum []     = 0
sum (x:xs) = x + (sum xs)

euler n = let xs = fromto 1 n
          in length (filter (relPrime n) xs)

main = print (sum (map euler (fromto 1 1000)))
\end{verbatim}
