module Components.Reflector
  ( Reflector (),
    reflector,
  )
where

import Characters (ValidChar, baseValidCharSet, validateCharSet)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))
import Control.Monad (when)

type Reflector = [(ValidChar, ValidChar)]

reflector :: String -> EnigmaMonad Reflector
reflector s =
  let validCharSetToReflector vcs = return $ do
        c <- vcs
        bc <- baseValidCharSet 
        -- check that it doesn't return the same character
        [(c, bc), (bc, c)]
   in validateCharSet s >>= validCharSetToReflector

--historicReflectorB
