import System.IO
import Data.Char

main :: IO ()
-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- ## use withFile to automatically close the file
-- main = do
--   withFile "girlfriend.txt" ReadMode $
--     \handle -> do
--       contents <- hGetContents handle
--       putStr contents

-- ## simpler again using readFile
-- main = do
--   contents <- readFile "girlfriend.txt"
--   putStr contents

-- ## let's write to a file now
main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" (map toUpper contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO b) -> IO b
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result
