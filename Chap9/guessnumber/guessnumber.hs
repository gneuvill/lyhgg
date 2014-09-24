import System.Random
import Control.Monad (when)

main :: IO ()
main = do
  putStr "Quel nombre ?\n> "
  l <- getLine
  when (l /= "STOP") $ do
    n <- toInt l
    g <- genNum
    if n == g
      then putStr "YES !!\n"
      else putStr ("Beuh non, c'est pas " ++ show n ++ "\n") >> main
  
toInt :: String -> IO Int
toInt = readIO

genNum :: (Random a, Num a) => IO a
genNum = fmap (fst . randomR (1, 3)) newStdGen
