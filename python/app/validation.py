import re

from settings import LETTERS


def prep_chars(string: str) -> str:
    return re.sub(r'\s+', '', string.upper())


def char_valid(char: str) -> bool:
    return len(char) == 1 and char in LETTERS
