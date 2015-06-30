{
  main 
    = sum (mapDefeuler (fromto 1 1000));
  
  sum x21 = case x21 of {
      Nil -> 0;
      Cons x22 x23 -> (x22 + sum x23)
      };
  
  mapDefeuler x9 = case x9 of {
      Nil -> Nil;
      Cons x10 x11 ->
        Cons (euler x10) (mapDefeuler x11)
      };
  
  fromto x5 x6
    = ifte ((x5 > x6)) Nil (Cons x5 (fromto ((x5 + 1)) x6));
  
  euler x24 = let 
      xs = fromto 1 x24
       in
      (length (filterDefrelPrime x24 xs));
  
  ifte x0 x1 x2 = case x0 of {
      True -> x1;
      False -> x2
      };
  
  length x18 = case x18 of {
      Nil -> 0;
      Cons x19 x20 -> (1 + length x20)
      };
  
  filterDefrelPrime x12 x15 = case
      x15 of {
      Nil -> Nil;
      Cons x16 x17 ->
        ifte (relPrime x12 x16) (Cons x16 (filterDefrelPrime x12 x17)) (filterDefrelPrime x12 x17)
      };
  
  relPrime x12 x13
    = (gcd x12 x13 == 1);
  
  gcd x3 x4
    = ifte ((x4 == 0)) x3 (ifte ((x3 > x4)) (gcd ((x3 - x4)) x4) (gcd x3 ((x4 - x3))))
}