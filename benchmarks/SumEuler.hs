myGcd x y = if y == 0 
          then x 
          else if x > y 
               then myGcd (x - y) y
               else myGcd x (y - x)

relPrime x y = myGcd x y == 1

euler n = length $ filter (relPrime n) xs
  where
    xs = [1..n]

main = print $ sum $ map euler [1..1000]
