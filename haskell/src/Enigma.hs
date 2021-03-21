module Enigma where

import Exceptions (EnigmaException)

type EnigmaMonad a = Either EnigmaException a
