module Components.Reflector where

import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))

import ValidCharacters

type Reflector = [ValidChar]

reflector :: [Char] -> Reflector
