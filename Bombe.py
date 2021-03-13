#!/usr/bin/python
# -*- coding: utf-8 -*-

# decrypts Enigma messages

import logging
import random
from collections import OrderedDict
from itertools import permutations
from Enigma import EnigmaMachine, Rotor

from Settings import *


def decode():
    pass


if __name__ == '__main__':
    rotor_permutations = list(permutations(ROTORS, 3))

    random_rotors = [Rotor(setting) for setting in random.choice(rotor_permutations)]
    random_message_key = random.choices(LETTERS, k=3)
    random_ring_settings = random.choices(LETTERS, k=3)
    random_plugboard_setting = []
    free_letters = list(LETTERS)
    for i in range(10):
        sample = random.sample(free_letters, 2)
        for c in sample:
            free_letters.remove(c)
        random_plugboard_setting.append(''.join(sample))

    logging.info(f'Random rotors: {random_rotors}')
    logging.info(f'Random message key: {random_message_key}')
    logging.info(f'Random ring setting: {random_ring_settings}')
    logging.info(f'Random plugboard setting: {random_plugboard_setting}')

    enigma = EnigmaMachine(rotors=random_rotors,
                           rotor_positions=random_message_key,
                           ring_settings=random_ring_settings,
                           plugboard_settings=random_plugboard_setting,
                           reflector=REFLECTOR_B_SETTINGS)

    testmessage = 'AAAAAA'
    # testmessage = 'EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA ' \
    #               'CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA ' \
    #               'SCTAC DPBOP VHJK'
    logging.info(f'Encrypting message: {testmessage}')
    result = enigma.encrypt_message(testmessage)
    logging.info(f'Result: {result}')

