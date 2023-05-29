import Data.List (intercalate, partition)

data FSTree
  = FileNode String Int
  | DirectoryNode String [FSTree]

data Command = Cd String | Ls

printFSTree :: FSTree -> String
printFSTree = go 0
  where
    go indent (FileNode name size) =
      replicate indent ' ' ++ "- " ++ name ++ " (file, size=" ++ show size ++ ")"
    go indent (DirectoryNode name contents) =
      replicate indent ' ' ++ "- " ++ name ++ " (dir)\n" ++ (intercalate "\n" (map (go $ indent + 2) contents))

newDirectory :: String -> FSTree
newDirectory dirName = DirectoryNode dirName []

newFile :: String -> Int -> FSTree
newFile fileName fileSize = FileNode fileName fileSize

matchDirectory :: String -> FSTree -> Bool
matchDirectory thatDirName (DirectoryNode thisDirName _) = thisDirName == thatDirName
matchDirectory _ _ = False

addFile :: FSTree -> [String] -> String -> Int -> FSTree
addFile (FileNode _ _) _ _ _ = error "Can't add a file to file"
addFile (DirectoryNode dirName contents) [] fileName fileSize =
  DirectoryNode dirName (newFile fileName fileSize : contents)
addFile (DirectoryNode dirName contents) (dir : dirs) fileName fileSize =
  let (matched, rest) = partition (matchDirectory dir) contents
      newDir = case matched of
        [] -> newDirectory dir
        ((DirectoryNode _ oldContent) : _) -> DirectoryNode dir oldContent
        _ -> error "Unexpected case while adding a file"
   in DirectoryNode dirName (addFile newDir dirs fileName fileSize : rest)

totalSize :: FSTree -> Int
totalSize (FileNode _ size) = size
totalSize (DirectoryNode _ contents) = sum $ map totalSize contents

directoriesAtLeast :: Int -> FSTree -> [(String, Int)]
directoriesAtLeast maxDirectorySize (FileNode _ _) = []
directoriesAtLeast maxDirectorySize (DirectoryNode dirName contents) =
  let thisDirectorySize = totalSize (DirectoryNode dirName contents)
      subDirectories = concatMap (directoriesAtLeast maxDirectorySize) contents
   in if thisDirectorySize <= maxDirectorySize
        then (dirName, thisDirectorySize) : subDirectories
        else subDirectories

sumDirectorySizes :: [(String, Int)] -> Int
sumDirectorySizes = sum . map snd

separateQueryFromOutput :: [String] -> [(String, [String])]
separateQueryFromOutput [] = []
separateQueryFromOutput (cmd : rest) =
  let (output, other) = break isCommand rest
   in (cmd, output) : separateQueryFromOutput other
  where
    isCommand cmd = head cmd == '$'

parseCommandStr :: String -> Command
parseCommandStr query =
  case words query of
    ["$", "cd", dir] -> Cd dir
    ["$", "cd"] -> Cd "/"
    ["$", "ls"] -> Ls
    _ -> error "Invalid command"

parseOutput :: [String] -> [(Int, String)]
parseOutput output = map (\str -> (read (head str), last str)) $ filter (not . isDirectory) $ map words output
  where
    isDirectory str = head str == "dir"

parseCommand :: (FSTree, [String]) -> (Command, [String]) -> (FSTree, [String])
parseCommand ((FileNode _ _), _) _ = error "Can't perform operation on a file"
parseCommand (root, path) (cmd, output) =
  case cmd of
    Cd "/" -> (root, [])
    Cd ".." -> (root, init path)
    Cd dir -> (root, path ++ [dir])
    Ls -> (foldl (\acc (size, name) -> addFile acc path name size) root $ parseOutput output, path)

main :: IO ()
main = do
  input <- separateQueryFromOutput . lines <$> readFile "input.txt"
  let root = newDirectory "/"
  let commands = map (\(query, output) -> (parseCommandStr query, output)) input
  let (fs, _) = foldl parseCommand (root, []) commands
  print . sumDirectorySizes $ (directoriesAtLeast 100000 fs)
