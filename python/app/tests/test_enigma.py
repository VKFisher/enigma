from enigma import Enigma
from components.historic import HistoricReflector, HistoricRotor
from components.rotor import RotorSet, RotorConfig, RotorSetConfig
from components.plugboard import Plugboard


def test_message_is_decrypted() -> None:
    # Arrange
    enigma = Enigma(
        rotors=RotorSet(
            first=HistoricRotor.II,
            second=HistoricRotor.IV,
            third=HistoricRotor.V,
        ),
        plugboard=Plugboard('AV BS CG DL FU HZ IN KM OW RX'),
        reflector=HistoricReflector.B,
    )
    enigma.configure_rotors(RotorSetConfig(
        first=RotorConfig(position='B', ring_setting='B'),
        second=RotorConfig(position='L', ring_setting='U'),
        third=RotorConfig(position='A', ring_setting='L'),
    ))

    input_message = 'EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA ' \
                    'GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA ' \
                    'TLPIF SVKDA SCTAC DPBOP VHJK'
    expected_output = 'AUFKLXABTEILUNGXVONXKURTINOWAXKURTINOWAXNORDWESTLXSEBEZXSEBEZXUAFFLIEGERSTRASZERIQTUNGX' \
                      'DUBROWKIXDUBROWKIXOPOTSCHKAXOPOTSCHKAXUMXEINSAQTDREINULLXUHRANGETRETENXANGRIFFXINFXRGTX'

    # Act
    actual_output = enigma.decrypt_message(input_message)

    # Assert
    assert actual_output == expected_output


def test_double_step_is_applied() -> None:
    # Arrange
    enigma = Enigma(
        rotors=RotorSet(
            first=HistoricRotor.III,
            second=HistoricRotor.II,
            third=HistoricRotor.I,
        ),
        reflector=HistoricReflector.B,
    )
    enigma.configure_rotors(RotorSetConfig(
        first=RotorConfig(position='K', ring_setting='A'),
        second=RotorConfig(position='D', ring_setting='A'),
        third=RotorConfig(position='O', ring_setting='A'),
    ))

    input_message = 'AAAAAA'
    expected_rotor_positions = ['L', 'F', 'U']

    # Act
    enigma.decrypt_message(input_message)

    # Assert
    actual_rotor_positions = [rotor.position for rotor in enigma.rotors]

    assert actual_rotor_positions == expected_rotor_positions
