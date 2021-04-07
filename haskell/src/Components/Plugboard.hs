module Components.Plugboard (
  plugboard
  , applyPlugboard
) where

import Common (getDuplicates)
import Control.Monad (liftM2, unless)
import Control.Monad.Cont (when)
import Data.Bifunctor (bimap)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))
import ValidCharacters

type Plugboard = [(ValidChar, ValidChar)]

charPairFromList :: [Char] -> EnigmaMonad (Char, Char)
charPairFromList [c1, c2] = Right (c1, c2)
charPairFromList s = Left . InvalidPlugboardInput $ s

valdiateCharPair :: (Char, Char) -> EnigmaMonad (ValidChar, ValidChar)
valdiateCharPair (c1, c2) = do
  vc1 <- validChar c1
  vc2 <- validChar c2
  when (vc1 == vc2) (Left . IdenticalCharsPair . toChar $ vc1)
  return (vc1, vc2)

sortCharPair :: (ValidChar, ValidChar) -> EnigmaMonad (ValidChar, ValidChar)
sortCharPair (vc1, vc2) = return $ if vc2 > vc1 then (vc1, vc2) else (vc2, vc1)

getValidCharPair :: [Char] -> EnigmaMonad (ValidChar, ValidChar)
getValidCharPair cs = charPairFromList cs >>= valdiateCharPair >>= sortCharPair

plugboard :: String -> EnigmaMonad Plugboard
plugboard s = charPairSetFromString s >>= validateCharPairSet
  where
    charPairSetFromString s' = traverse getValidCharPair . words $ s'
    validateCharPairSet charPairs = do
      let pairCount = length charPairs
      when (pairCount > 10) (Left . ExcessPlugboardPairCount $ pairCount)
      let duplicates = getDuplicates charPairs
      unless (null duplicates) (Left . DuplicatePlugboardPairs $ (bimap toChar toChar <$> duplicates))
      return charPairs

applyPlugboard' :: Plugboard -> ValidChar -> ValidChar
applyPlugboard' ((c1, c2) : ps) c
  | c == c1 = c2
  | c == c2 = c1
  | otherwise = applyPlugboard' ps c
applyPlugboard' [] c = c

applyPlugboard :: EnigmaMonad Plugboard -> EnigmaMonad ValidChar -> EnigmaMonad ValidChar
applyPlugboard = liftM2 applyPlugboard'
