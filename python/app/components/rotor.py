from collections import Counter
from dataclasses import dataclass

from exceptions import RotorException
from characters import prep_chars, char_valid, CHARACTERS


class Rotor:
    CHARACTER_COUNT = len(CHARACTERS)

    def __init__(self, wiring, turnovers):
        self._ring_setting = 0
        self._offset = 0

        self._turnovers = self._parse_turnovers(turnovers)
        self._forward_map, self._backward_map = self._parse_wiring(wiring)

        self._forward_map = {i: CHARACTERS.index(char) for i, char in enumerate(wiring)}
        self._backward_map = {i: wiring.index(char) for i, char in enumerate(CHARACTERS)}

    def _parse_turnovers(self, turnovers):
        _turnovers = prep_chars(turnovers)

        if invalid_chars := {
            char
            for char in turnovers
            if not char_valid(char)
        }:
            raise RotorException(f'Invalid characters: {invalid_chars}')

        if duplicate_chars := {
            char
            for char, count in Counter(_turnovers).items()
            if count > 1
        }:
            raise RotorException(f'Duplicate characters: {duplicate_chars}')

        if len(_turnovers) > self.CHARACTER_COUNT:
            raise RotorException(
                f'Expected at most {self.CHARACTER_COUNT} characters in turnover settings, got {len(_turnovers)}'
            )

        return [CHARACTERS.index(char) for char in _turnovers]

    def _parse_wiring(self, wiring):
        _wiring = prep_chars(wiring)

        if invalid_chars := {
            char
            for char in _wiring
            if not char_valid(char)
        }:
            raise RotorException(f'Invalid characters in wiring settings: {invalid_chars}')

        if duplicate_characters := {
            char
            for char, count in Counter(_wiring).items()
            if count > 1
        }:
            raise RotorException(f'Duplicate characters in wiring settings: {duplicate_characters}')

        if len(_wiring) != self.CHARACTER_COUNT:
            raise RotorException(f'Expected {self.CHARACTER_COUNT} characters in wiring settings, got {len(_wiring)}')

        forward_map = {i: CHARACTERS.index(char) for i, char in enumerate(_wiring)}
        backward_map = {i: _wiring.index(char) for i, char in enumerate(CHARACTERS)}

        return forward_map, backward_map

    @staticmethod
    def _char_to_index(char):
        char = prep_chars(char)

        if not char_valid(char):
            raise RotorException(f'Invalid character: {char}')

        return CHARACTERS.index(char)

    @property
    def ring_setting(self):
        return CHARACTERS[self._ring_setting]

    @property
    def position(self):
        return CHARACTERS[self._offset]

    @ring_setting.setter
    def ring_setting(self, value):
        self._ring_setting = self._char_to_index(value)

    @position.setter
    def position(self, value):
        self._offset = self._char_to_index(value)

    @property
    def in_turnover_position(self):
        return self._offset in self._turnovers

    def step(self):
        self._offset = (self._offset + 1) % self.CHARACTER_COUNT

    def _apply(self, char: str, backward: bool) -> str:
        char_index = self._char_to_index(char)
        char_index = (char_index - self._ring_setting + self._offset + self.CHARACTER_COUNT) % self.CHARACTER_COUNT
        char_index = self._backward_map[char_index] if backward else self._forward_map[char_index]
        char_index = (char_index + self._ring_setting - self._offset + self.CHARACTER_COUNT) % self.CHARACTER_COUNT
        return CHARACTERS[char_index]

    def apply_forward(self, char: str) -> str:
        return self._apply(char, backward=False)

    def apply_backward(self, char: str) -> str:
        return self._apply(char, backward=True)


@dataclass
class RotorSet:
    first: Rotor
    second: Rotor
    third: Rotor

    def __post_init__(self):
        self._rotors = [self.first, self.second, self.third]

    def __getitem__(self, position):
        return self._rotors[position]

    def __len__(self):
        return len(self._rotors)


@dataclass
class RotorConfig:
    position: str
    ring_setting: str


@dataclass
class RotorSetConfig:
    first: RotorConfig
    second: RotorConfig
    third: RotorConfig

    def __post_init__(self):
        self._rotor_configs = [self.first, self.second, self.third]

    def __getitem__(self, position):
        return self._rotor_configs[position]

    def __len__(self):
        return len(self._rotor_configs)
