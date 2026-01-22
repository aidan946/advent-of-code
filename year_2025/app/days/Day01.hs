module Days.Day01 (printAnswer) where

newtype DialNum = DialNum Int deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

data Direction = R | L deriving (Show, Eq)

data Movement = Movement
  { direction :: Direction,
    amount :: Int
  }
  deriving (Show, Eq)

charToDirection :: Char -> Direction
charToDirection 'R' = R
charToDirection 'L' = L
charToDirection _ = L

intToDialNum :: Int -> DialNum
intToDialNum = DialNum

dialNumToInt :: DialNum -> Int
dialNumToInt (DialNum n) = n

parseMovement :: String -> Movement
parseMovement (x : xs) = Movement (charToDirection x) (read xs :: Int)
parseMovement _ = Movement L 0

printAnswer :: IO ()
printAnswer = do
  fileArray <- splitFile
  result1 <- listIteratorPart1 fileArray (50, 0)
  print result1
  result2 <- listIteratorPart2 fileArray (50, 0)
  print result2

splitFile :: IO [String]
splitFile = lines <$> readFile "inputs/input_01.txt"

listIteratorPart1 :: [String] -> (DialNum, Int) -> IO (DialNum, Int)
listIteratorPart1 [] (dialNum, clickSum) = return (dialNum, clickSum)
listIteratorPart1 [x] (dialNum, clickSum) = do
  print (dialNum, clickSum)
  let newDialNum = calculateAdjustmentPart1 (parseMovement x) dialNum
  if newDialNum == 0
    then return (newDialNum, clickSum + 1)
    else return (newDialNum, clickSum)
listIteratorPart1 (x : xs) (dialNum, clickSum) = do
  print (dialNum, clickSum)
  let newDialNum = calculateAdjustmentPart1 (parseMovement x) dialNum
  if newDialNum == 0
    then listIteratorPart1 xs (newDialNum, clickSum + 1)
    else listIteratorPart1 xs (newDialNum, clickSum)

calculateAdjustmentPart1 :: Movement -> DialNum -> DialNum
calculateAdjustmentPart1 (Movement L movementAmount) dialNum =
  intToDialNum (mod (dialNumToInt dialNum - movementAmount) 100)
calculateAdjustmentPart1 (Movement R movementAmount) dialNum =
  intToDialNum (mod (dialNumToInt dialNum + movementAmount) 100)

listIteratorPart2 :: [String] -> (DialNum, Int) -> IO (DialNum, Int)
listIteratorPart2 [] (dialNum, clickSum) = return (dialNum, clickSum)
listIteratorPart2 [x] (dialNum, clickSum) = do
  print (dialNum, clickSum)
  return calculateAdjustmentPart2 (parseMovement x) (dialNum, clickSum)
listIteratorPart2 (x : xs) (dialNum, clickSum) = do
  print (dialNum, clickSum)
  listIteratorPart1 xs (calculateAdjustmentPart1 (parseMovement x) (dialNum, clickSum))

calculateAdjustmentPart2 :: Movement -> (DialNum, Int) -> (DialNum, Int)
calculateAdjustmentPart2 (Movement L movementAmount) (dialNum, clickSum) =
  (intToDialNum (mod (dialNumToInt dialNum - movementAmount) 100), clickSum)
calculateAdjustmentPart2 (Movement R movementAmount) (dialNum, clickSum) =
  (intToDialNum (mod (dialNumToInt dialNum + movementAmount) 100), clickSum)
