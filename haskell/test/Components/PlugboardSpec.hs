{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Components.PlugboardSpec where

import Components.Plugboard
import Exceptions (EnigmaException (..))
import Test.Framework
import ValidCharacters

test_validPlugboardIsProcessed :: IO ()
test_validPlugboardIsProcessed = assertEqual expected actual
  where
    expected = Right [(ValidChar 'A', ValidChar 'B'), (ValidChar 'C', ValidChar 'D')]
    actual = plugboardFromString "AB CD"

test_validPlugboardWithLowercaseCharsIsProcessed :: IO ()
test_validPlugboardWithLowercaseCharsIsProcessed = assertEqual expected actual
  where
    expected = Right [(ValidChar 'A', ValidChar 'B'), (ValidChar 'C', ValidChar 'D')]
    actual = plugboardFromString "Ab cD"

test_emptyPlugboardIsProcessed :: IO ()
test_emptyPlugboardIsProcessed = assertEqual expected actual
  where
    expected = Right []
    actual = plugboardFromString ""

test_plugboardWithInvalidSymbolsIsRejected :: IO ()
test_plugboardWithInvalidSymbolsIsRejected = assertEqual expected actual
  where
    expected = Left . InvalidChar $ 'Ы'
    actual = plugboardFromString "AB CD EF GH ЫЖ KL MN OP QR ST"

test_plugboardWithExcessPairsIsRejected :: IO ()
test_plugboardWithExcessPairsIsRejected = assertEqual expected actual
  where
    expected = Left . ExcessPlugboardPairCount $ 13
    actual = plugboardFromString "AB CD EF GH IJ KL MN OP QR ST UV WX YZ"

test_plugboardWithRepeatingPairsIsRejected :: IO ()
test_plugboardWithRepeatingPairsIsRejected = assertEqual expected actual
  where
    expected = Left . DuplicatePlugboardPairs $ [('A', 'B'), ('G', 'H'), ('I', 'J')]
    actual = plugboardFromString "AB CD AB GH IJ IJ KL MN GH"

test_plugboardWithIdenticalCharacterPairsIsRejected :: IO ()
test_plugboardWithIdenticalCharacterPairsIsRejected = assertEqual expected actual
  where
    expected = Left . IdenticalCharsPair $ 'A'
    actual = plugboardFromString "AB CD AA GH IJ"

test_plugboardIsAppliedCorrectly :: IO ()
test_plugboardIsAppliedCorrectly = assertEqual expected actual
  where
    pgSettings = "AB CD EF"
    inputs = "ABFGK"
    outputs = "BAEGK"
    expected = Right . ValidChar <$> outputs
    actual = applyPlugboard (plugboardFromString pgSettings) . toValidChar <$> inputs
