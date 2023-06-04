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

move :: (Position, Position) -> Direction -> (Position, Position)
move (tPos, hPos) dir =
  let newHead = moveHead hPos dir
      newTail = moveTowards tPos newHead
   in (newTail, newHead)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let parsed = map parseInstruction input
      repeated = concatMap (\(dir, rep) -> replicate rep dir) parsed
      positions = scanl move ((0, 0), (0, 0)) repeated
      tailPositions = nub $ map fst positions
  print $ length tailPositions