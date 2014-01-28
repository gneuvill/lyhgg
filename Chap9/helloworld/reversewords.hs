
main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

action :: IO ()
action = do
  rs <- sequence $ map print [1, 2, 3, 4, 5]
  print rs
