from settings import LETTERS, ROTORS


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
