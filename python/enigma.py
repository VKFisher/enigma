# An implementation of the Enigma encryption machine

import logging

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
            reflector,
            plugboard_settings=None
    ):
        self.change_settings(
            rotors=rotors,
            rotor_positions=rotor_positions,
            ring_settings=ring_settings,
            plugboard_settings=plugboard_settings,
            reflector=reflector
        )

    def change_settings(self, rotors=None, rotor_positions=None, ring_settings=None,
                        reflector=None, plugboard_settings=None):
        # the convention is to count rotors from left to right
        if rotors:
            assert len(rotors) == 3
            self._rotors = rotors

        if rotor_positions:
            self._initial_rotor_positions = rotor_positions

        if ring_settings:
            for i in range(len(self._rotors)):
                self._rotors[i].ring_setting = ring_settings[i]

        if plugboard_settings:
            assert len(plugboard_settings) <= 10
            self._plugboard = {}
            for pair in plugboard_settings:
                assert len(pair) == 2
                pair = pair.upper()
                assert (pair[0] in LETTERS) and (pair[1] in LETTERS)
                assert (pair[0] not in self._plugboard) and (pair[1] not in self._plugboard)

                self._plugboard[pair[0]] = pair[1]
                self._plugboard[pair[1]] = pair[0]

        if reflector:
            assert len(reflector) == len(LETTERS)
            self._reflector = {LETTERS[i]: reflector[i] for i in range(len(LETTERS))}

    def _process_message(self, message):
        encrypted_message = []

        # set initial rotor positions
        for i in range(len(self._rotors)):
            self._rotors[i].set_ring_position(self._initial_rotor_positions[i])

        for letter in message.upper():
            if letter in LETTERS:
                encrypted_message.append(self.encrypt_letter(letter))

        return ''.join(encrypted_message)

    encrypt_message = _process_message
    decrypt_message = _process_message

    def encrypt_letter(self, letter):
        encrypted_letter = letter

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
        if encrypted_letter in self._plugboard:
            encrypted_letter = self._plugboard[encrypted_letter]

        # rotors - forward
        for rotor in self._rotors[::-1]:
            encrypted_letter = rotor.scramble(encrypted_letter)

        # reflector
        encrypted_letter = self._reflector[encrypted_letter]

        # rotors - backward
        for rotor in self._rotors:
            encrypted_letter = rotor.scramble(encrypted_letter, reverse=True)

        # plugboard - backward
        if encrypted_letter in self._plugboard:
            encrypted_letter = self._plugboard[encrypted_letter]

        # get result
        return encrypted_letter


class Rotor:
    _position = None

    @property
    def position(self):
        return self._position

    @position.setter
    def position(self, value):
        value = LETTERS.index(value)
        assert 25 >= value >= 0
        self._position = value

    @property
    def ring_position(self):
        return LETTERS[(self.position + self.ring_setting) % 26]

    @property
    def ring_setting(self):
        return self._ring_setting

    @ring_setting.setter
    def ring_setting(self, value):
        value = LETTERS.index(value)
        assert 25 >= value >= 0
        self._ring_setting = value

    @property
    def rotate_next_trigger_position(self):
        return (self._rotate_next_trigger_position - self.ring_setting) % 26

    @property
    def connections(self):
        return self._connections

    @property
    def rotate_next(self):
        # adjusted_rotation_position = (self.rotate_next_trigger_position + self.ring_setting) % 26
        return self.position == self.rotate_next_trigger_position

    def __init__(self, rotor_wiring, step_trigger, ring_setting):
        # check rotor number

        self.number = 0
        for i in range(len(ROTORS)):
            if rotor_wiring == ROTORS[i].rotor_wiring:
                self.number = i + 1

        # set up rotor wiring connections
        self._connections = {LETTERS[i]: rotor_wiring[i] for i in range(len(LETTERS))}
        self._connections_reversed = {value: key for key, value in self._connections.items()}

        # set up position that triggers the rotation of another rotor
        self._rotate_next_trigger_position = LETTERS.index(step_trigger)
        self.ring_setting = ring_setting

        # set initial postion
        self.set_ring_position('A')

    def __repr__(self):
        return f'rotor {self.number}'

    def set_ring_position(self, pos):
        assert pos in LETTERS
        self._position = (LETTERS.index(pos) - self.ring_setting) % 26

    def scramble(self, letter, reverse=False):
        assert letter in LETTERS
        assert len(letter) == 1

        # Account for shifted position
        pos_index = (LETTERS.index(letter) + self.position) % 26

        letter = LETTERS[pos_index]
        if not reverse:
            # get scrambled letter
            scrambled_letter = self._connections[letter]
        else:
            # get scrambled letter
            scrambled_letter = self._connections_reversed[letter]

        # Account for shifted position again
        pos_index = (LETTERS.index(scrambled_letter) - self.position) % 26

        return LETTERS[pos_index]
        # return scrambled_letter

    def rotate(self):
        self._position += 1
        if self._position > 25:
            self._position = 0


if __name__ == "__main__":
    enigma = EnigmaMachine(
        rotors=[Rotor(*ROTOR_2_SETTINGS), Rotor(*ROTOR_4_SETTINGS), Rotor(*ROTOR_5_SETTINGS)],
        rotor_positions='BLA',
        ring_settings=[LETTERS[1], LETTERS[20], LETTERS[11]],
        plugboard_settings='AV BS CG DL FU HZ IN KM OW RX'.split(),
        reflector=REFLECTOR_B_SETTINGS
    )

    # testmessage = 'AAAAAA'
    testmessage = 'EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA ' \
                  'CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA ' \
                  'SCTAC DPBOP VHJK'
    logging.info(f'Encrypting message: {testmessage}')
    result = enigma.encrypt_message(testmessage)
    print(result)
    logging.info(f'Result: {" ".join(result.split("X"))}')
