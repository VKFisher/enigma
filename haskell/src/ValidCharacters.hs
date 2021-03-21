module ValidCharacters where

import Control.Monad.Except
import Data.Char (toUpper)
import Exceptions (EnigmaException (..))
import Enigma (EnigmaMonad)

validCharacterList :: [Char]
validCharacterList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

newtype ValidChar = ValidChar Char deriving (Eq, Show, Ord)

validChar :: Char -> EnigmaMonad ValidChar
validChar c
  | uc `elem` validCharacterList = Right $ ValidChar uc
  | otherwise = Left $ InvalidChar c
  where
    uc = toUpper c

toChar :: ValidChar -> Char
toChar (ValidChar c) = c

toValidCharString :: String -> EnigmaMonad [ValidChar]
toValidCharString = traverse validChar