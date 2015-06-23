\centering
\begin{BVerbatim}
tak :: Int -> Int -> Int -> Int
tak x y z = case x <= y of
                True  -> z
                False -> tak (tak (x - 1) y z)
                             (tak (y - 1) z x)
                             (tak (z - 1) x y)

main = tak 24 16 8
\end{BVerbatim}
