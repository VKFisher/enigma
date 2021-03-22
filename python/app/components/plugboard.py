from collections import Counter

from exceptions import PlugboardException
from characters import char_valid, prep_chars, CHARACTERS

MAX_PAIRS = 10


class Plugboard:
    def __init__(self, wiring: str):
        self._swap_map = self._parse_wiring(wiring)

    @staticmethod
    def _parse_wiring(settings):
        pair_strings = [prep_chars(pair_string) for pair_string in settings.split()]

        if invalid_chars := {
            char
            for pair_string in pair_strings
            for char in pair_string
            if not char_valid(char)
        }:
            raise PlugboardException(f'Invalid characters: {invalid_chars}')

        if invalid_pair_strings := {
            s
            for s in pair_strings
            if len(s) != 2
        }:
            raise PlugboardException(f'Invalid pair strings: {invalid_pair_strings}')

        pairs = [tuple(sorted(pair)) for pair in pair_strings]

        if duplicate_pairs := {
            ''.join(list(pair))
            for pair, count in Counter(pairs).items()
            if count > 1
        }:
            raise PlugboardException(f'Duplicate pairs: {duplicate_pairs}')

        if identical_char_pairs := {
            pair
            for pair in pairs
            if pair[0] == pair[1]
        }:
            raise PlugboardException(f'Identical characters in pairs: {identical_char_pairs}')

        if (pair_amount := len(pairs)) > MAX_PAIRS:
            raise PlugboardException(f'Expected at most {MAX_PAIRS} pairs, got {pair_amount}')

        swap_map = {
            **{c: c for c in CHARACTERS},
            **{c1: c2 for c1, c2 in pairs},
            **{c2: c1 for c1, c2 in pairs}
        }

        return swap_map

    def apply(self, char: str):
        if char not in self._swap_map:
            raise PlugboardException(f'Invalid character: {char}')

        return self._swap_map[char.upper()]
