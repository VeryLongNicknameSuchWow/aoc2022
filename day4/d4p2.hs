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

overlap :: [(Int, Int)] -> Bool
overlap [(a, b), (c, d)] = max a c <= min b d
overlap _ = error "Two intervals expected"

main :: IO ()
main = do
  inputLines <- lines <$> readFile "input.txt"
  let intervals = (map parseInterval . splitListBySep ',') <$> inputLines
  print . length . filter overlap $ intervals