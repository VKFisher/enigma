module Components.Plugboard where

import Common (getDuplicates)
import Control.Monad (liftM2)
import Control.Monad.Cont (when)
import Data.Bifunctor (bimap)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))
import ValidCharacters

type Plugboard = [(ValidChar, ValidChar)]

valdiateCharPair :: (Char, Char) -> EnigmaMonad (ValidChar, ValidChar)
valdiateCharPair (c1, c2) = do
  vc1 <- toValidChar c1
  vc2 <- toValidChar c2
  when (vc1 == vc2) (Left . IdenticalCharsPair . toChar $ vc1)
  return (vc1, vc2)

charPairFromList :: [Char] -> EnigmaMonad (Char, Char)
charPairFromList [c1, c2] = Right (c1, c2)
charPairFromList s = Left . InvalidPlugboardInput $ s

getValidCharPair :: [Char] -> EnigmaMonad (ValidChar, ValidChar)
getValidCharPair cs = charPairFromList cs >>= valdiateCharPair

plugboard :: [(ValidChar, ValidChar)] -> EnigmaMonad Plugboard
plugboard charPairs
  | pairCount > 10 = Left . ExcessPlugboardPairCount $ pairCount
  | not (null duplicates) = Left . DuplicatePlugboardPairs $ (bimap toChar toChar <$> duplicates)
  | otherwise = Right charPairs
  where
    pairCount = length charPairs
    duplicates = getDuplicates charPairs

plugboardFromString :: String -> EnigmaMonad Plugboard
plugboardFromString s = traverse getValidCharPair (words s) >>= plugboard

applyPlugboard_ :: Plugboard -> ValidChar -> ValidChar
applyPlugboard_ ((c1, c2) : ps) c
  | c == c1 = c2
  | c == c2 = c1
  | otherwise = applyPlugboard_ ps c
applyPlugboard_ [] c = c

applyPlugboard :: EnigmaMonad Plugboard -> EnigmaMonad ValidChar -> EnigmaMonad ValidChar
applyPlugboard = liftM2 applyPlugboard_
