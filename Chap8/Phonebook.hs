module Chap8.Phonebook where

import qualified Data.Map as Map

 -- type synonyms (aliases)
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- parameterized type synonyms

type AssocList k v = [(k , v)]

-- partially applied (like functions)

type AssocListString v = AssocList String v
-- or
-- type AssocListString' = AssocList String -- Wrong ! Partially applied *type synonyms* are NOT allowed !
type IntMap = Map.Map Int -- this (IntMap) is a synonym of a partially applied *type* (Map is not a synonym)


