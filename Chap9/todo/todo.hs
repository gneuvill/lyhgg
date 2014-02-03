import Data.List
import System.Environment
import System.IO
import System.Directory

view :: [String] -> IO ()
view (path:_) = do
  content <- readFile path
  putStr $ unlines . zipWith (\i s -> show i ++ ". " ++ s) [0..] . lines $ content

add :: [String] -> IO ()
add (_:[]) = putStrLn "An argument is missing (item) !"
add (path:item:_) = appendFile path (item ++ "\n")

treatItem :: [String] -> (String -> [String] -> [String]) -> IO ()
treatItem (_:[]) _ = putStrLn "An argument is missing (index) !"
treatItem (path:index:_) f = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  content <- readFile path
  let items = lines content
      newItems = f (items !! read index) items 
  hPutStr tempHandle $ unlines newItems
  hClose tempHandle
  removeFile path
  renameFile tempName path

remove :: [String] -> IO ()
remove l = treatItem l delete

bump :: [String] -> IO ()
bump l = treatItem l (\x xs -> x : delete x xs)

commands :: [(String, [String] -> IO ())]
commands = [ ("view", view)
           , ("add", add)
           , ("remove", remove)
           , ("bump", bump)
           ]

syntax :: IO ()
syntax = do
  prg <- getProgName
  putStrLn $ "Syntax : " ++ prg ++ " action file [arg]"

main :: IO ()
main = do
  args <- getArgs
  case args of [] -> syntax
               (_:[]) -> syntax
               (action:supArgs) ->
                 case lookup action commands of
                   Just cmd -> cmd supArgs
                   Nothing -> putStrLn $ "Action " ++ action ++ " unknown !"
