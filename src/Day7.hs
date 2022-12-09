module Main where

import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Text.ParserCombinators.ReadP ((<++))
import Text.ParserCombinators.ReadP qualified as P

-- File system data types

-- | A file with a name and a size.
data File = File {fName :: String, fSize :: Int}

instance Show File where
  show (File name size) = name <> "(file, size=" <> show size <> ")"

-- | A directory with a name, a size and a list of files and directories contained in it.
data Dir = Dir
  { dName :: String
  , dSize :: Int
  , dFiles :: Map.Map String File
  , dDirs :: Map.Map String Dir
  }

instance Show Dir where
  showsPrec d (Dir name _ files dirs) =
    showString (concat $ replicate d "  ")
      . showString "— "
      . showString name
      . showString " (dir)\n"
      . showDirs (sortOn dName $ Map.elems dirs)
      . showFiles (sortOn fName $ Map.elems files)
    where
      showDirs :: [Dir] -> ShowS
      showDirs = foldr (.) id . fmap (showsPrec $ d + 1)

      showFiles :: [File] -> ShowS
      showFiles = foldr ((.) . showFile) id

      showFile :: File -> ShowS
      showFile (File name' size) =
        showString (concat $ replicate (d + 1) "  ")
          . showString "— "
          . showString name'
          . showString " (file, size="
          . shows size
          . showString ")\n"

-- | Creates an empty directory with the given name.
emptyDir :: String -> Dir
emptyDir name = Dir name 0 Map.empty Map.empty

-- Parser data types

-- | Arguments to the 'cd' command.
data CdArg = CdDir String | CdUp | CdRoot deriving (Show)

-- | Commands that can be executed on the file system: 'cd' and 'ls'.
data Command = Cd CdArg | Ls deriving (Show)

-- | Output of the 'ls' command: a file or a directory.
data Output = OutputFile File | OutputDir Dir deriving (Show)

-- | A line of input: a command or one line of the output of a previous command.
data Line = LCommand Command | LOutput Output deriving (Show)

-- Parser

-- | Parser for a command.
commandParser :: P.ReadP Command
commandParser = P.char '$' *> P.skipSpaces *> P.choice [Cd <$> cdParser, Ls <$ lsParser]
  where
    lsParser = P.string "ls"
    cdParser =
      P.string "cd"
        *> P.skipSpaces
        *> ((CdUp <$ P.string "..") <++ (CdRoot <$ P.string "/") <++ (CdDir <$> P.munch1 (/= ' ')))

-- | Parser for a line of command output.
outputParser :: P.ReadP Output
outputParser = P.choice [OutputFile <$> fileParser, OutputDir <$> dirParser]
  where
    fileParser = flip File <$> (read <$> P.munch1 isDigit) <*> (P.skipSpaces *> P.munch1 (/= ' '))
    dirParser = emptyDir <$> (P.string "dir " *> P.munch1 (/= ' '))

-- | Parser for a line of the browsing
lineParser :: P.ReadP Line
lineParser = P.choice [LOutput <$> outputParser, LCommand <$> commandParser]

-- | Parses a line of the browsing session.
parseLine :: String -> Line
parseLine s = case P.readP_to_S lineParser s of
  [(l, "")] -> l
  _ -> error $ "Failed to parse line: " <> s

-- File system zipper data types and functions

-- | A zipper for a file system.
data FsZipper = FsZipper {zPath :: [Dir], zCurrent :: Dir} deriving (Show)

-- | Moves up a directory in the file system.
moveUp :: FsZipper -> FsZipper
moveUp = \case
  FsZipper [] _ -> error "Can't move up from root"
  FsZipper (d : ds) cur -> FsZipper ds $ d {dDirs = Map.insert (dName cur) cur $ dDirs d}

-- | Moves down a directory in the file system.
moveDown :: String -> FsZipper -> FsZipper
moveDown name (FsZipper ds d) = FsZipper (d : ds) $ findDir d
  where
    findDir Dir {dDirs = ds'} = case Map.lookup name ds' of
      Nothing -> error $ "Can't find directory " <> name
      Just d' -> d'

-- | Moves to the root directory in the file system.
moveToRoot :: FsZipper -> FsZipper
moveToRoot zipper = case zipper of
  FsZipper [] _ -> zipper
  _ -> moveToRoot $ moveUp zipper

-- | Adds a file to the current directory in the file system.
addFile :: File -> FsZipper -> FsZipper
addFile f (FsZipper ds d) = FsZipper ds d {dFiles = Map.insert (fName f) f $ dFiles d}

addDir :: Dir -> FsZipper -> FsZipper
addDir d (FsZipper ds d') = FsZipper ds d' {dDirs = Map.insert (dName d) d $ dDirs d'}

-- | Converts the root directory of the file system to a zipper.
toZipper :: Dir -> FsZipper
toZipper = FsZipper []

-- | Converts a zipper to the root directory of the file system.
fromZipper :: FsZipper -> Dir
fromZipper = zCurrent . moveToRoot

-- Interpreter

-- | Interprets a line of the input, updating the file system zipper.
interpretLine :: FsZipper -> Line -> FsZipper
interpretLine zipper = \case
  LCommand (Cd CdUp) -> moveUp zipper
  LCommand (Cd CdRoot) -> moveToRoot zipper
  LCommand (Cd (CdDir name)) -> moveDown name zipper
  LCommand Ls -> zipper
  LOutput (OutputFile f) -> addFile f zipper
  LOutput (OutputDir d) -> addDir d zipper

-- | Interprets a list of lines of input, returning the root directory of the final file system.
interpret :: [Line] -> Dir
interpret = fromZipper . foldl interpretLine (toZipper $ emptyDir "/")

-- Main

-- | Calculates and sets the size of a directory.
calcAndSetDirSize :: Dir -> Dir
calcAndSetDirSize d@Dir {dFiles = fs, dDirs = ds} =
  let ds' = fmap calcAndSetDirSize ds
   in d {dSize = sum $ fmap fSize fs <> fmap dSize ds', dDirs = ds'}

-- | Finds the directories smaller than the given size.
findDirsSmallerThan :: Int -> Dir -> [Dir]
findDirsSmallerThan size d@(Dir {dSize = dSize', dDirs = ds}) =
  [d | dSize' <= size] <> concatMap (findDirsSmallerThan size) ds

-- | Finds the directories larger than the given size.
findDirsLargerThan :: Int -> Dir -> [Dir]
findDirsLargerThan size d@(Dir {dSize = dSize', dDirs = ds}) =
  [d | dSize' >= size] <> concatMap (findDirsLargerThan size) ds

-- | Part 1 of the solution: returns the sum of the sizes of directories smaller than 100000.
part1 :: Dir -> Int
part1 = sum . map dSize . findDirsSmallerThan 100000

-- | Part 2 of the solution: returns the size of the smallest directory larger than space required for the update.
part2 :: Dir -> Int
part2 fs =
  let freeSpace = totalSpace - dSize fs
      spaceRequired = updateSpace - freeSpace
      dirs = findDirsLargerThan spaceRequired fs
   in minimum $ map dSize dirs
  where
    totalSpace = 70000000
    updateSpace = 30000000

main :: IO ()
main = do
  input <- lines <$> getContents
  let fs = calcAndSetDirSize $ interpret $ map parseLine input
  print fs
  print $ part1 fs
  print $ part2 fs
