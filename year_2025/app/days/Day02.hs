module Days.Day02 (printAnswer) where

printAnswer :: IO ()
printAnswer = do
  fileArray <- splitFile
  print (listIterator 0 fileArray)

splitFile :: IO [String]
splitFile = lines <$> readFile "inputs/input_02.txt"

listIterator :: Int -> [String] -> Int
listIterator iterSum [] = iterSum
listIterator iterSum [x] = iterSum
listIterator iterSum (x : xs) = listIterator iterSum xs
