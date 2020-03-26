import Control.Monad
import System.IO

io1 = 
    getLine
    >>= \line -> return(length line)
    >>= \len -> putStrLn(show len)

io2 =
    putStrLn "What your name?" 
    >>= \_ -> getLine
    >>= \name -> putStrLn $ "Welcome " ++ name ++ " to FP"
io3 =
    return "crash"
    >>= \a -> return " or not"
    >>= \b -> putStrLn(a ++ b)

io4 =
    do
        line <- getLine
        len <- return (length line)
        putStrLn (show len)

io5 = do
    line <- getLine
    let len = length line
    putStrLn (show len)

io6 = getLine >>= return . length >>= putStrLn . show

putCharEx =
    putChar 'G'
    >> putChar 'G'
    >> putChar 'E'
    -- _ <- putChar 'Z'

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

-- print :: Show a => a -> IO ()
printEX = do
    print True
    print "GG"

getCharEx = do 
    c <- getChar
    if c /= ' '
        then do putChar c
                getCharEx
        else return ()

----------------Function form control monad

---------if
when' p s = if p then s else pure ()

getCharEx' = do 
    c <- getChar
    when' (c /= ' ') $ do
        putChar c
        getCharEx'

print3 = do
    lines <- sequence [getLine, getLine, getLine]
    print lines

print5 = do
    sequence (map print [1 .. 5])

print5' = do
    mapM_ print [1 .. 5]

-- PleaseRunIt = forever $ do
    -- print "error!!!"

forEx = do
    forM [1,2,3,4] (\a -> do
        putStr $ "Enter number " ++ show a ++ ":"
        num <- getLine
        return (read num :: Int))

    >>= \nums ->
        putStrLn $ "Sum of numbers is "++ show (sum nums)

-- file I/O

fileEx = do
    h <- openFile "scaled_shapes.pgm" ReadMode
    contents <- hGetContents h
    putStr contents
    hClose h

