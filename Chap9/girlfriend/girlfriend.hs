import System.IO

main :: IO ()
main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
                    
