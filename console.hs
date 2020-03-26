import System.Console.ANSI  --Console output
import System.IO
import Data.List
-- Set colors and write some text in those colors.
main :: IO ()
main = do

    -- clearScreen     --clear all

    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetColor Background Vivid Blue]
    putStrLn "Red-On-Blue"

    setSGR [Reset]  -- Reset to default colour scheme
    putStrLn "Default colors."

