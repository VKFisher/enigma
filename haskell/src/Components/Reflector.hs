module Components.Reflector where

import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))
import ValidCharacters (ValidChar)

type Reflector = [(ValidChar, ValidChar)]

--reflector :: String -> Reflector
--historicReflectorB
