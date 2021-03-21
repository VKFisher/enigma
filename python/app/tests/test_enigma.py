from enigma import EnigmaMachine
from components.historic import HistoricReflector, HistoricRotor
from components.rotor import RotorSet, RotorConfig, RotorSetConfig
from components.plugboard import Plugboard


def test_message_is_decrypted():
    # Arrange
    enigma = EnigmaMachine(
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
