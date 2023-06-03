import Data.Char (digitToInt)
import Data.List (transpose)

scenicScore :: [Int] -> [Int]
scenicScore [] = []
scenicScore (t : xs) = score : scenicScore xs
  where
    (a, b) = break (>= t) xs
    score = if null b then length a else length a + 1

calculateScores :: [[Int]] -> [[Int]]
calculateScores = map scenicScore

calculateScoresRev :: [[Int]] -> [[Int]]
calculateScoresRev = map (reverse . scenicScore . reverse)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let numericInput = map digitToInt <$> input
      transposedInput = transpose numericInput
      a = calculateScores numericInput
      b = calculateScoresRev $ numericInput
      c = transpose . calculateScores $ transposedInput
      d = transpose . calculateScoresRev $ transposedInput
      product = foldl1 (zipWith (zipWith (*))) [a, b, c, d]
      result = maximum $ maximum <$> product
  print result
