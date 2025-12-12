module Days.Day01 (printAnswer) where

printAnswer :: IO ()
printAnswer = splitFile >>= calculateAnswer

splitFile :: IO [String]
splitFile = lines <$> readFile "inputs/input_01.txt"

calculateAnswer :: [String] -> IO ()
calculateAnswer [] = return ()
calculateAnswer [x] = putStrLn x
calculateAnswer (x : xs) = do
  putStrLn x
  calculateAnswer xs

splitString :: String -> (Char, Int)
splitString (x : xs) = (x, read xs :: Int)
