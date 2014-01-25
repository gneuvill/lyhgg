import Data.Char

main :: IO ()
main = do
  putStrLn "Hello, what's your first name ?"
  firstName <- getLine
  putStrLn "what's your last name ?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", you rock !"

-- main = putStrLn "Hello, what's your name ?" >>=
--        (\_ -> getLine >>=
--               (\name -> putStrLn $ "Hey " ++ name ++ ", you rock !"))
