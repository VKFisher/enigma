module ValidCharacters where

import Data.Char (toUpper)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))

validCharacterList :: [Char]
validCharacterList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

newtype ValidChar = ValidChar Char deriving (Eq, Show, Ord)

toValidChar :: Char -> EnigmaMonad ValidChar
toValidChar c
  | uc `elem` validCharacterList = Right $ ValidChar uc
  | otherwise = Left $ InvalidChar c
  where
    uc = toUpper c

toChar :: ValidChar -> Char
toChar (ValidChar c) = c

toValidCharString :: String -> EnigmaMonad [ValidChar]
toValidCharString = traverse toValidChar
