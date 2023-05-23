import Data.List (nub)

uniqueChars :: String -> Bool
uniqueChars str = (length . nub) str == length str

processChar :: (Bool, Int, String) -> Char -> (Bool, Int, String)
processChar (True, count, str) _ = (True, count, str)
processChar (False, count, str) char =
  if uniqueChars str
    then (True, count, str)
    else (False, count + 1, tail str ++ [char])

main :: IO ()
main = do
  input <- head . lines <$> readFile "input.txt"
  let (h, t) = splitAt 4 input
  let (found, index, str) = foldl processChar (False, 4, h) t
  print index