import Data.List (nub)

type Position = (Int, Int)

type Direction = Char

type Instruction = (Direction, Int)

parseInstruction :: String -> Instruction
parseInstruction (dir : strNum) =
  case dir of
    'U' -> (dir, read strNum)
    'D' -> (dir, read strNum)
    'L' -> (dir, read strNum)
    'R' -> (dir, read strNum)
    _ -> error $ "Invalid direction: " ++ [dir]
parseInstruction _ = error "Invalid instruction"

moveHead :: Position -> Direction -> Position
moveHead (hX, hY) dir =
  case dir of
    'R' -> (hX + 1, hY)
    'L' -> (hX - 1, hY)
    'U' -> (hX, hY + 1)
    'D' -> (hX, hY - 1)
    _ -> error $ "Invalid direction: " ++ [dir]

moveTowards :: Position -> Position -> Position
moveTowards (tX, tY) (hX, hY) =
  let dX = hX - tX
      dY = hY - tY
      aX = abs dX
      aY = abs dY
      len = aX + aY
      mX = signum dX
      mY = signum dY
   in if len > 2
        then (tX + mX, tY + mY)
        else
          if aX > 1
            then (tX + mX, tY)
            else
              if aY > 1
                then (tX, tY + mY)
                else (tX, tY)

moveHelper :: [Position] -> Position -> [Position]
moveHelper [] _ = []
moveHelper (current : rest) towards =
  let newCurrent = moveTowards current towards
      newRest = moveHelper rest newCurrent
   in newCurrent : newRest

move :: [Position] -> Direction -> [Position]
move [] _ = []
move (h : t) dir =
  let newHead = moveHead h dir
      newTail = moveHelper t newHead
   in newHead : newTail

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let instructions = map parseInstruction input
      replicated = concatMap (\(dir, rep) -> replicate rep dir) instructions
      ropeLength = 10
      rope = replicate ropeLength (0, 0)
      ropeStates = scanl move rope replicated
      tailPositions = map last ropeStates
      uniqueTailPositions = nub tailPositions
  print . length $ uniqueTailPositions