module ValidCharacters (
  ValidChar ()
  , validChar
  , toChar
  , baseCharacterList
) where

import Data.Char (toUpper)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))

validCharacterList :: [Char]
validCharacterList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

baseCharacterList :: [ValidChar]
baseCharacterList = ValidChar <$> validCharacterList

newtype ValidChar = ValidChar Char deriving (Eq, Show, Ord)

validChar :: Char -> EnigmaMonad ValidChar
validChar c
  | uc `elem` validCharacterList = Right $ ValidChar uc
  | otherwise = Left $ InvalidChar c
  where
    uc = toUpper c

toChar :: ValidChar -> Char
toChar (ValidChar c) = c

