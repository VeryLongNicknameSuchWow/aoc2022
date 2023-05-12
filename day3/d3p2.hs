import Data.Char (ord)
import Data.List (intersect, nub)

calculatePriority :: Char -> Int
calculatePriority c
  | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
  | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 27
  | otherwise = error "Invalid item"

stringIntersection :: (String, String, String) -> [Char]
stringIntersection (str1, str2, str3) = nub $ intersect str1 (intersect str2 str3)

toTriples :: [a] -> [(a, a, a)]
toTriples [] = []
toTriples (x : y : z : rest) = (x, y, z) : toTriples rest
toTriples _ = error "List not divisable by 3"

readFileToLines :: FilePath -> IO [String]
readFileToLines filePath = lines <$> readFile filePath

main :: IO ()
main = do
  inputLines <- readFileToLines "input.txt"
  let triples = toTriples inputLines
  let intersections = map stringIntersection triples
  let priorities = map (head . map calculatePriority) intersections
  print $ sum $ priorities