failCourse :: (Ord a, Num a) => a -> Bool
failCourse score = score < 80

failCourse' :: (Ord a, Num a) => a -> (Bool,[Char])
failCourse' score = (score < 80, "failCoure Call")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f =
    let (y, flog) = f x
    in (y , log ++ flog)