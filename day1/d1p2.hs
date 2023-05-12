import Data.List (sort)

splitListPred :: (a -> Bool) -> [a] -> [[a]]
splitListPred _ [] = []
splitListPred pred items = do
  let (h, t) = break pred items
  [h] ++ splitListPred pred (drop 1 t)

splitListBySep :: Eq a => a -> [a] -> [[a]]
splitListBySep sep items = splitListPred (sep ==) items

readFileToLines :: FilePath -> IO [String]
readFileToLines filePath = lines <$> readFile filePath

readInt :: String -> Integer
readInt = read

main :: IO ()
main = do
  inputLines <- readFileToLines "input.txt"
  let splitStrs = splitListBySep "" inputLines
  let splitNums = (readInt <$>) <$> splitStrs
  let splitSums = sum <$> splitNums
  putStrLn $ show $ sum $ take 3 $ reverse $ sort splitSums
