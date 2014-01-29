import Control.Monad

main :: IO ()
-- main = do
--   contents <- getContents
--   putStr $ (unlines . filter ((<10) . length) . lines) contents
main = interact $ unlines . filter ((<10) . length) . lines
