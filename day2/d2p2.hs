calculatePoints :: Num p => String -> p
calculatePoints "A X" = 3 + 0
calculatePoints "A Y" = 1 + 3
calculatePoints "A Z" = 2 + 6
calculatePoints "B X" = 1 + 0
calculatePoints "B Y" = 2 + 3
calculatePoints "B Z" = 3 + 6
calculatePoints "C X" = 2 + 0
calculatePoints "C Y" = 3 + 3
calculatePoints "C Z" = 1 + 6
calculatePoints _ = 0

readFileToLines :: FilePath -> IO [String]
readFileToLines filePath = lines <$> readFile filePath

main :: IO ()
main = do
  inputLines <- readFileToLines "input.txt"
  let results = map calculatePoints inputLines
  putStrLn $ show $ sum results
