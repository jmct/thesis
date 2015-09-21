\centering
\begin{BVerbatim}
tak x y z = case x <= y of
                True  -> z
                False -> let x' = tak ((x - 1)) y z
                             y' = tak ((y - 1)) z x
                             z' = tak ((z - 1)) x y
                         in (par x'         -- par-site 0
                                (par y'     -- par-site 1 
                                    (seq z' 
                                        (tak x' y' z'))))
    
main  = tak 24 16 8
\end{BVerbatim}
