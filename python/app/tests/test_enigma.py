from enigma import EnigmaMachine, Rotor
from components.plugboard import Plugboard
from components.reflector import REFLECTOR_B
from settings import *


def test_message_is_decrypted():
    # Arrange
    enigma = EnigmaMachine(
        rotors=[Rotor(*ROTOR_2_SETTINGS), Rotor(*ROTOR_4_SETTINGS), Rotor(*ROTOR_5_SETTINGS)],
        rotor_positions='BLA',
        ring_settings=[LETTERS[1], LETTERS[20], LETTERS[11]],
        plugboard=Plugboard('AV BS CG DL FU HZ IN KM OW RX'),
        reflector=REFLECTOR_B,
    )

    input_message = 'EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA ' \
                    'GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA ' \
                    'TLPIF SVKDA SCTAC DPBOP VHJK'
    expected_output = 'AUFKLXABTEILUNGXVONXKURTINOWAXKURTINOWAXNORDWESTLXSEBEZXSEBEZXUAFFLIEGERSTRASZERIQTUNGX' \
                      'DUBROWKIXDUBROWKIXOPOTSCHKAXOPOTSCHKAXUMXEINSAQTDREINULLXUHRANGETRETENXANGRIFFXINFXRGTX'
    # Act
    actual_output = enigma.decrypt_message(input_message)

    # Assert
    assert actual_output == expected_output
