from collections import Counter

from exceptions import ReflectorException
from characters import prep_chars, char_valid, CHARACTERS


class Reflector:
    def __init__(self, wiring):
        self._swap_map = self._parse_wiring(wiring)

    @staticmethod
    def _parse_wiring(wiring):
        _wiring = prep_chars(wiring)

        if invalid_chars := {
            char
            for char in _wiring
            if not char_valid(char)
        }:
            raise ReflectorException(f'Invalid characters: {invalid_chars}')

        if duplicate_characters := {
            char
            for char, count in Counter(_wiring).items()
            if count > 1
        }:
            raise ReflectorException(f'Duplicate characters: {duplicate_characters}')

        if len(_wiring) != len(CHARACTERS):
            raise ReflectorException(f'Expected {len(CHARACTERS)} characters, got {len(_wiring)}')

        swap_map = ({
            **{letter: _wiring[i] for i, letter in enumerate(CHARACTERS)},
            **{_wiring[i]: letter for i, letter in enumerate(CHARACTERS)}
        })

        return swap_map

    def apply(self, char: str):
        if char not in self._swap_map:
            raise ReflectorException(f'Invalid character: {char}')

        return self._swap_map[char.upper()]
