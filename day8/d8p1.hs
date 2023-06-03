import Data.Char (digitToInt)
import Data.List (transpose)

-- I need to parse each row from both sides.
-- This function reverses each row while marking. (prepend to accumulator)
markTrees :: [(Int, Bool)] -> [(Int, Bool)]
markTrees = snd . foldl process ((-1), [])
  where
    process (maxSoFar, acc) (n, b) =
      let isLargest = b || n > maxSoFar
       in (max maxSoFar n, (n, isLargest) : acc)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let parsed = map (\str -> (digitToInt str, False)) <$> input
      horizontallyMarked = map (markTrees . markTrees) parsed
      verticallyMarked = map (markTrees . markTrees) . transpose $ horizontallyMarked
      result = length . filter snd . concat $ verticallyMarked
  print result
