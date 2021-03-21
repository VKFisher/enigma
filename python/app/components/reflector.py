from collections import Counter

from app.exceptions import ReflectorException
from app.settings import LETTERS
from app.validation import prep_chars, char_valid


class Reflector:
    def __init__(self, settings):
        self._parse_settings(settings)

    def _parse_settings(self, settings):
        chars = prep_chars(settings)

        if invalid_chars := {
            char
            for char in chars
            if not char_valid(char)
        }:
            raise ReflectorException(f'Invalid characters in reflector settings: {invalid_chars}')

        if duplicate_characters := {
            char
            for char, count in Counter(chars).items()
            if count > 1
        }:
            raise ReflectorException(f'Duplicate characters: {duplicate_characters}')

        if len(chars) != len(LETTERS):
            raise ReflectorException(f'Expected {len(chars)} characters, got {len(LETTERS)}')

        self._swap_map = ({
            **{letter: chars[i] for i, letter in enumerate(LETTERS)},
            **{chars[i]: letter for i, letter in enumerate(LETTERS)}
        })

    def apply(self, char: str):
        if char not in self._swap_map:
            raise ReflectorException(f'Invalid character: {char}')

        return self._swap_map[char.upper()]


REFLECTOR_B = Reflector('YRUHQSLDPXNGOKMIEBFZCWVJAT')
REFLECTOR_C = Reflector('FVPJIAOYEDRZXWGCTKUQSBNMHL')
