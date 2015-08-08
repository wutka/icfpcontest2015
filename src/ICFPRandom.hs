module ICFPRandom where

randomList :: Int -> [Int]
randomList seed = 
    seed : randomList nextSeed
        where
          nextSeed = (seed * 1103515245 + 12345) `mod` (2^32)

randomNumbers :: Int -> [Int]
randomNumbers seed = map extractRandom (randomList seed)
    where
      extractRandom n = (n `div` (2^16)) `mod` (2^15)

main = do
  putStrLn . unwords $ map show (take 10 (randomList 17))
  
