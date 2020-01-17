---------------------startWith------------------------
startWith [] ys = True
startWith xs [] = False
startWith (x:xs) (y:ys) = (x == y) && startWith xs ys

---------------------endWith--------------------------
endWith x y = startWith (reverse x) (reverse y)