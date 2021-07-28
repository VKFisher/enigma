{-# LANGUAGE DataKinds #-}

module Components.Historic where

import Components.Reflector
  ( Reflector,
    reflector,
  )

data HistoricReflectorType = A | B | C | BThin | CThin

historicReflector :: HistoricReflectorType -> Reflector
historicReflector A = reflector "EJMZALYXVBWFCRQUONTSPIKHGD"
historicReflector B = reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT"
historicReflector C = reflector "FVPJIAOYEDRZXWGCTKUQSBNMHL"
historicReflector BThin = reflector "ENKQAUYWJICOPBLMDXZVFTHRGS"
historicReflector CThin = reflector "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

--class HistoricReflector:
--    A = Reflector('EJMZALYXVBWFCRQUONTSPIKHGD')
--    B = Reflector('YRUHQSLDPXNGOKMIEBFZCWVJAT')
--    C = Reflector('FVPJIAOYEDRZXWGCTKUQSBNMHL')
--    BThin = Reflector('ENKQAUYWJICOPBLMDXZVFTHRGS')
--    CThin = Reflector('RDOBJNTKVEHMLFCWZAXGYIPSUQ')

--class HistoricRotor:
--    I = Rotor("EKMFLGDQVZNTOWYHXUSPAIBRCJ", turnovers="Q")  # noqa
--    II = Rotor("AJDKSIRUXBLHWTMCQGZNPYFVOE", turnovers="E")
--    III = Rotor("BDFHJLCPRTXVZNYEIWGAKMUSQO", turnovers="V")
--    IV = Rotor("ESOVPZJAYQUIRHXLNFTGKDCMWB", turnovers="J")
--    V = Rotor("VZBRGITYUPSDNHLXAWMJQOFECK", turnovers="Z")
--    VI = Rotor("JPGVOUMFYQBENHZRDKASXLICTW", turnovers="ZM")
--    VII = Rotor("NZJHGRCXMYSWBOUFAIVLPEKQDT", turnovers="ZM")
--    VIII = Rotor("FKQHTLXOCBJSPDZRAMEWNIUYGV", turnovers="ZM")
