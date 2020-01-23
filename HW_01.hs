---------------------startWith------------------------
startWith [] _ = True
startWith _ [] = False
startWith (x:xs) (y:ys) = (x == y) && startWith xs ys

---------------------endWith--------------------------
endWith x y = startWith (reverse x) (reverse y)

---------------------countSub-------------------------
countSub xs ys
    | xs == [] = 0
    | otherwise = countSub' xs ys 0

    where   countSub' _ [] res = res
            countSub' xs ys res
                | startWith xs ys = countSub' xs (tail ys) (res + 1)
                | otherwise = countSub' xs (tail ys) res

--------------------transpose-------------------------
transpose x
    | elem 0 (map length x) = []
    | otherwise = (map head x) : transpose (map tail x)