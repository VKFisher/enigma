from collections import namedtuple

# Enigma machine settings

# letters supported by the machine
LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

# original rotor settings
RotorSettings = namedtuple(
    typename='RotorSettings',
    field_names=('rotor_wiring', 'step_trigger', 'ring_setting')
)

#                           pins: ABCDEFGHIJKLMNOPQRSTUVWXYZ
ROTOR_1_SETTINGS = RotorSettings('EKMFLGDQVZNTOWYHXUSPAIBRCJ', 'R', 'A')
ROTOR_2_SETTINGS = RotorSettings('AJDKSIRUXBLHWTMCQGZNPYFVOE', 'F', 'A')
ROTOR_3_SETTINGS = RotorSettings('BDFHJLCPRTXVZNYEIWGAKMUSQO', 'W', 'A')
ROTOR_4_SETTINGS = RotorSettings('ESOVPZJAYQUIRHXLNFTGKDCMWB', 'K', 'A')
ROTOR_5_SETTINGS = RotorSettings('VZBRGITYUPSDNHLXAWMJQOFECK', 'A', 'A')

ROTORS = (ROTOR_1_SETTINGS, ROTOR_2_SETTINGS, ROTOR_3_SETTINGS, ROTOR_4_SETTINGS, ROTOR_5_SETTINGS)

TEST_ROTOR = ('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'X', 'A')

# original reflector setings

REFLECTOR_B_SETTINGS = 'YRUHQSLDPXNGOKMIEBFZCWVJAT'
REFLECTOR_C_SETTINGS = 'FVPJIAOYEDRZXWGCTKUQSBNMHL'

TEST_REFLECTOR = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
