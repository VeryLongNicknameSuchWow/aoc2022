splitListPred :: (a -> Bool) -> [a] -> [[a]]
splitListPred _ [] = []
splitListPred pred items = do
  let (h, t) = break pred items
  h : splitListPred pred (drop 1 t)

splitListBySep :: Eq a => a -> [a] -> [[a]]
splitListBySep sep items = splitListPred (sep ==) items

parseInterval :: String -> (Int, Int)
parseInterval s =
  let [a, b] = map read $ splitListBySep '-' s
   in (a, b)

fullyContains :: [(Int, Int)] -> Bool
fullyContains [(a, b), (c, d)] = (a >= c && b <= d) || (a <= c && b >= d)
fullyContains _ = error "Two intervals expected"

main :: IO ()
main = do
  inputLines <- lines <$> readFile "input.txt"
  let intervals = (map parseInterval . splitListBySep ',') <$> inputLines
  print . length . filter fullyContains $ intervals