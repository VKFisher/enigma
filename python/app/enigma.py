# An implementation of the Enigma encryption machine

from funcy import compose, rcompose

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
        f = compose(*[rotor.apply_forward for rotor in self.rotors])
        return f(char)

    def _apply_rotors_backward(self, char: str) -> str:
        f = rcompose(*[rotor.apply_backward for rotor in self.rotors])
        return f(char)

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
        f = rcompose(*[
            self._plugboard.apply,
            self._apply_rotors_forward,
            self._reflector.apply,
            self._apply_rotors_backward,
            self._plugboard.apply,
        ])

        return f(char)
