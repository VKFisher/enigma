# An implementation of the Enigma encryption machine

import logging

from components.plugboard import Plugboard
from components.reflector import Reflector, REFLECTOR_B
from components.rotor import Rotor
from settings import *

logging.basicConfig(level=logging.DEBUG, format=' %(asctime)s - %(levelname)s - %(message)s')
logging.disable(logging.DEBUG)

logger = logging.getLogger('enigma')


class EnigmaMachine:
    _rotors = None
    _initial_rotor_positions = None
    _plugboard = {}
    _reflector = None

    def __init__(
            self,
            rotors,
            rotor_positions,
            ring_settings,
            reflector: Reflector,
            plugboard: Plugboard = Plugboard("")
    ):
        self._plugboard = plugboard
        self._reflector = reflector
        self.change_settings(
            rotors=rotors,
            rotor_positions=rotor_positions,
            ring_settings=ring_settings
        )

    def change_settings(self, rotors=None, rotor_positions=None, ring_settings=None):
        # the convention is to count rotors from left to right
        if rotors:
            assert len(rotors) == 3
            self._rotors = rotors

        if rotor_positions:
            self._initial_rotor_positions = rotor_positions

        if ring_settings:
            for i in range(len(self._rotors)):
                self._rotors[i].ring_setting = ring_settings[i]

    def _process_message(self, message):
        encrypted_message = []

        # set initial rotor positions
        for i in range(len(self._rotors)):
            self._rotors[i].set_ring_position(self._initial_rotor_positions[i])

        for letter in message.upper():
            if letter in LETTERS:
                encrypted_message.append(self.encrypt_letter(letter))

        return ''.join(encrypted_message)

    encrypt_message = decrypt_message = _process_message

    def encrypt_letter(self, letter):
        # rotate rotors
        count = 0
        for rotor in self._rotors[::-1]:
            count += 1
            rotor.rotate()

            logging.debug(f'Rotated rotor #{count}')

            # double step handlng
            # if rotor #2 was rotated into its turnover position on the previous iteration
            # this time it will rotate again independend of rotor #3
            rotor_is_second = rotor == self._rotors[2]
            double_step_trigger = self._rotors[1].position - self._rotors[1].rotate_next_trigger_position == -1

            if not (rotor.rotate_next or rotor_is_second and double_step_trigger):
                break

        logging.debug(f'Rotor ring positions after rotation: '
                      f'{"".join([rotor.ring_position for rotor in self._rotors])}')

        # plugboard - forward
        letter = self._plugboard.apply(letter)

        # rotors - forward
        for rotor in self._rotors[::-1]:
            letter = rotor.scramble(letter)

        # reflector
        letter = self._reflector.apply(letter)

        # rotors - backward
        for rotor in self._rotors:
            letter = rotor.scramble(letter, reverse=True)

        # plugboard - backward
        letter = self._plugboard.apply(letter)

        # get result
        return letter


if __name__ == "__main__":
    enigma = EnigmaMachine(
        rotors=[Rotor(*ROTOR_2_SETTINGS), Rotor(*ROTOR_4_SETTINGS), Rotor(*ROTOR_5_SETTINGS)],
        rotor_positions='BLA',
        ring_settings=[LETTERS[1], LETTERS[20], LETTERS[11]],
        plugboard=Plugboard('AV BS CG DL FU HZ IN KM OW RX'),
        reflector=REFLECTOR_B,
    )

    # testmessage = 'AAAAAA'
    testmessage = 'EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA ' \
                  'CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA ' \
                  'SCTAC DPBOP VHJK'
    logging.info(f'Encrypting message: {testmessage}')
    result = enigma.encrypt_message(testmessage)
    print(result)
    logging.info(f'Result: {" ".join(result.split("X"))}')
