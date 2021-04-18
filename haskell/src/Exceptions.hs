module Exceptions where

import Data.Char (toUpper)

data EnigmaException
  = BlankException
  | GenericException String
  | InvalidChar Char
  | InvalidCharSet [Char]
  | InvalidCharSetLength Int
  | DuplicateCharsInCharSet [Char]
  | IdenticalCharsInPair Char
  | InvalidPlugboardInput String
  | ExcessPlugboardPairCount Int
  | DuplicatePlugboardPairs [(Char, Char)]
  deriving (Eq)

instance Show EnigmaException where
  show BlankException = "An uspecified error has occured"
  show (GenericException msg) = msg
  show (InvalidChar c) = "Invalid character: " ++ show c
  show (IdenticalCharsInPair c) = "Characters in pair are identical: " ++ ['"', uc, uc, '"'] where uc = toUpper c
  show (InvalidPlugboardInput s) = "Cannot convert plugboard setting input to character pair: " ++ s
  show (ExcessPlugboardPairCount cnt) = "Plugboard settings have " ++ show cnt ++ " pairs, maximuim is 10"
  show (DuplicatePlugboardPairs ps) =
    "Duplicate pairs in plugboard settings: " ++ concat ((\(c1, c2) -> [c1, c2]) <$> ps)
  show _ = "Unknown exception"
