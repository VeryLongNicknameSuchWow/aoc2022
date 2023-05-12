import Data.Char (ord)
import Data.List (intersect, nub)

calculatePriority :: Char -> Int
calculatePriority c
  | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
  | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 27
  | otherwise = error "Invalid item"

stringIntersection :: String -> String -> [Char]
stringIntersection str1 str2 = nub $ intersect str1 str2

splitStringInHalf :: String -> (String, String)
splitStringInHalf str = splitAt (length str `div` 2) str

readFileToLines :: FilePath -> IO [String]
readFileToLines filePath = lines <$> readFile filePath

main :: IO ()
main = do
  inputLines <- readFileToLines "input.txt"
  let compartments = map splitStringInHalf inputLines
  let intersections = map (uncurry stringIntersection) compartments
  let priorities = map (map calculatePriority) intersections
  print $ sum $ map maximum priorities