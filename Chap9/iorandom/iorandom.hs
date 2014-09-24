import System.Random


oneRString :: IO String
oneRString = do
  gen <- getStdGen -- always the same generator and thus...
  return $ take 20 $ randomRs ('a', 'z') gen -- ...always the same string

twoRString :: IO (String, String)
twoRString = do
  gen <- getStdGen
  return $ splitAt 20 (take 40 $ randomRs ('a', 'z') gen)
  
twoRString' :: IO (String, String)
twoRString' = do
  gen <- getStdGen
  gen2 <- newStdGen
  let rStr = take 20 . randomRs ('a', 'z')
  return (rStr gen, rStr gen2)
