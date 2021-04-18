module Characters
  ( ValidChar (),
    ValidCharSet (),
    validateChar,
    validateCharSet,
    baseValidCharSet,
  )
where

import Common (getDuplicates)
import Control.Monad (unless, when)
import Data.Char (toUpper)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))

type ValidChar = Char
type ValidCharSet = [ValidChar]

baseValidCharSet :: ValidCharSet
baseValidCharSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

validateChar :: Char -> EnigmaMonad ValidChar
validateChar c = do
  let uc = toUpper c
  unless (uc `elem` baseValidCharSet) (Left . InvalidChar $ c)
  return uc

validateCharSet :: String -> EnigmaMonad ValidCharSet
validateCharSet s = do
  cs <- traverse validateChar s
  let charSetLength = length cs
      baseCharSetLength = length baseValidCharSet
   in when (charSetLength /= baseCharSetLength) (Left . InvalidCharSetLength $ charSetLength)
  let duplicates = getDuplicates cs
   in unless (null duplicates) (Left . DuplicateCharsInCharSet $ duplicates)
  return cs