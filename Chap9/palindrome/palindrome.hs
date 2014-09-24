
main :: IO ()
main = interact $ unlines . map respondPalindrome . lines
  where
    respondPalindrome s | s == reverse s = "palindrome"
                        | otherwise = "not palindrome"
                                      
-- main = interact $
--        unlines
--        . map (\str ->
--                case str of s | isPalindrome s -> "palindrome"
--                              | otherwise -> "not palindrome")
--        . lines
--   where
--     isPalindrome str = str == reverse str

-- main = interact $ unlines . map (\s -> if isPalindrome s
--                                        then "palindrome"
--                                        else "not palindrome") . lines
--   where isPalindrome str = str == reverse str
