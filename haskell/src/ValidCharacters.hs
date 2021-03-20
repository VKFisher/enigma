module ValidCharacters where

import Data.Maybe (fromMaybe)


validCharacterList :: [Char]
validCharacterList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 

type ValidChar = Char

isValid :: Char -> Bool
isValid = (`elem` validCharacterList)

validChar' :: Char -> Maybe ValidChar
validChar' c
  | isValid c = Just c
  | otherwise = Nothing
  
validChar c = fromMaybe