# An implementation of the Enigma encryption machine

from components.plugboard import Plugboard
from components.reflector import Reflector
from components.rotor import RotorSet, RotorSetConfig
from characters import prep_chars, char_valid


class Enigma:
    def __init__(
            self,
            rotors: RotorSet,
            reflector: Reflector,
            plugboard: Plugboard = Plugboard("")
    ):
        self._plugboard = plugboard
        self._reflector = reflector
        self.rotors = rotors

    def configure_rotors(self, config: RotorSetConfig):
        for rotor, settings in zip(self.rotors, config):
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
        for rotor in self.rotors[::-1]:
            char = rotor.apply_forward(char)

        return char

    def _apply_rotors_backward(self, char: str) -> str:
        for rotor in self.rotors:
            char = rotor.apply_backward(char)

        return char

    def _step_rotors(self):
        step_first = self.rotors.second.in_turnover_position
        step_second = self.rotors.third.in_turnover_position or self.rotors.second.in_turnover_position

        if step_first:
            self.rotors.first.step()

        if step_second:
            self.rotors.second.step()

        self.rotors.third.step()

    def _process_char(self, char: str) -> str:
        # step rotors
        self._step_rotors()

        # apply encryption steps
        char = self._plugboard.apply(char)
        char = self._apply_rotors_forward(char)
        char = self._reflector.apply(char)
        char = self._apply_rotors_backward(char)
        char = self._plugboard.apply(char)

        return char
