-- f :: a -> a
-- f x = x

-- f :: a -> b -> a
-- f x y = x

-- f :: a -> b
-- y = 3
-- f x = y

-- f :: (a -> b) -> Maybe a -> Maybe b
-- f x = g x

-- decreasingPair :: (Ord a, Integral b) => [a] -> b
-- decreasingPair x = 

-- data RPNElem = N Int | OA | OS | OM deriving (show)
-- prn 
-- rpn x:xs = x 

--dec :: (Ord a, Integral b) => [a] -> b
dec x = dec' x 0

dec' [] res = res
dec' (x:xs) res
    | x > (head xs) = dec' xs (res + 1)
    | otherwise = dec' xs res