import System.Random

random1 = do
    take 10 $ randomRs ('a','z') $ mkStdGen 2547

main = do
  g <- newStdGen
  print $ take 10 (randomRs ('a', 'z') g)