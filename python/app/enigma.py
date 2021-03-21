# An implementation of the Enigma encryption machine

import logging

from components.plugboard import Plugboard
from components.reflector import Reflector
from components.rotor import RotorSet, RotorSetConfig
from validation import prep_chars, char_valid

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
            rotors: RotorSet,
            reflector: Reflector,
            plugboard: Plugboard = Plugboard("")
    ):
        self._plugboard = plugboard
        self._reflector = reflector
        self._rotors = rotors

    def configure_rotors(self, config: RotorSetConfig):
        for rotor, settings in zip(self._rotors, config):
            rotor.ring_setting = settings.ring_setting
            rotor.position = settings.position

    def _process_message(self, message):
        return ''.join([
            self._process_char(char)
            for char in prep_chars(message)
            if char_valid(char)
        ])

    encrypt_message = decrypt_message = _process_message

    def _apply_rotors_forward(self, char: str) -> str:
        for rotor in self._rotors[::-1]:
            char = rotor.apply_forward(char)

        return char

    def _apply_rotors_backward(self, char: str) -> str:
        for rotor in self._rotors:
            char = rotor.apply_backward(char)

        return char

    def _step_rotors(self):
        count = 0
        for rotor in self._rotors[::-1]:
            count += 1
            rotor.step()

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

    def _process_char(self, char: str) -> str:
        # rotate rotors
        self._step_rotors()

        # apply encryption steps
        char = self._plugboard.apply(char)
        char = self._apply_rotors_forward(char)
        char = self._reflector.apply(char)
        char = self._apply_rotors_backward(char)
        char = self._plugboard.apply(char)

        return char
