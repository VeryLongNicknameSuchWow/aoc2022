import Data.Char (isSpace)
import Data.List (transpose)

splitListPred :: (a -> Bool) -> [a] -> [[a]]
splitListPred _ [] = []
splitListPred pred items =
  let (h, t) = break pred items
   in h : splitListPred pred (drop 1 t)

splitListBySep :: Eq a => a -> [a] -> [[a]]
splitListBySep sep items = splitListPred (sep ==) items

parseInstruction :: String -> (Int, Int, Int)
parseInstruction str = case words str of
  ["move", a, "from", b, "to", c] -> (read a, read b - 1, read c - 1)
  _ -> error ""

parseState :: [String] -> [String]
parseState =
  map (filter (`notElem` " "))
    . filter (not . all isSpace)
    . map (filter (`notElem` "[]"))
    . transpose
    . init

changeState :: [String] -> (Int, Int, Int) -> [String]
changeState stacks (n, src, dest) =
  let (popped, rest) = splitAt n (stacks !! src)
      appended = reverse popped ++ stacks !! dest
   in replaceListElement src rest $ replaceListElement dest appended stacks

replaceListElement :: Int -> a -> [a] -> [a]
replaceListElement i x xs = take i xs ++ [x] ++ drop (i + 1) xs

main :: IO ()
main = do
  inputLines <- lines <$> readFile "input.txt"
  let [initialInput, instructionsInput] = splitListBySep "" inputLines
  let instructions = map parseInstruction instructionsInput
  let initialState = parseState initialInput
  let finalState = foldl changeState initialState instructions
  let result = map head finalState
  putStrLn result