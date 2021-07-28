{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Components.PlugboardSpec where

import Characters (ValidChar, fromChar, fromValidChar)
import Components.Plugboard
import Control.Monad (liftM)
import Enigma (EnigmaMonad)
import Exceptions (EnigmaException (..))
import Test.Framework

unpackPairList :: EnigmaMonad [(ValidChar, ValidChar)] -> EnigmaMonad [(Char, Char)]
unpackPairList =
  let unpack ((vc1, vc2) : ps) = (fromValidChar vc1, fromValidChar vc2) : unpack ps
      unpack [] = []
   in liftM unpack

test_validPlugboardIsProcessed :: IO ()
test_validPlugboardIsProcessed = assertEqual expected actual
  where
    expected = Right [('A', 'B'), ('C', 'D')]
    actual = unpackPairList . plugboard $ "AB CD"

test_validPlugboardWithLowercaseCharsIsProcessed :: IO ()
test_validPlugboardWithLowercaseCharsIsProcessed = assertEqual expected actual
  where
    expected = Right [('A', 'B'), ('C', 'D')]
    actual = unpackPairList . plugboard $ "Ab cD"

test_emptyPlugboardIsProcessed :: IO ()
test_emptyPlugboardIsProcessed = assertEqual expected actual
  where
    expected = Right []
    actual = plugboard ""

test_plugboardWithInvalidSymbolsIsRejected :: IO ()
test_plugboardWithInvalidSymbolsIsRejected = assertEqual expected actual
  where
    expected = Left . InvalidChar $ 'Ы'
    actual = plugboard "AB CD EF GH ЫЖ KL MN OP QR ST"

test_plugboardWithExcessPairsIsRejected :: IO ()
test_plugboardWithExcessPairsIsRejected = assertEqual expected actual
  where
    expected = Left . ExcessPlugboardPairCount $ 13
    actual = plugboard "AB CD EF GH IJ KL MN OP QR ST UV WX YZ"

test_plugboardWithRepeatingPairsIsRejected :: IO ()
test_plugboardWithRepeatingPairsIsRejected = assertEqual expected actual
  where
    expected = Left . DuplicatePlugboardPairs $ [('A', 'B'), ('G', 'H'), ('I', 'J')]
    actual = plugboard "AB CD AB GH IJ IJ KL MN GH"

test_plugboardWithIdenticalCharacterPairsIsRejected :: IO ()
test_plugboardWithIdenticalCharacterPairsIsRejected = assertEqual expected actual
  where
    expected = Left . IdenticalCharsInPair $ 'A'
    actual = plugboard "AB CD AA GH IJ"

test_plugboardIsAppliedCorrectly :: IO ()
test_plugboardIsAppliedCorrectly = assertEqual expected actual
  where
    pgSettings = "AB CD EF"
    inputs = "ABFGK"
    outputs = "BAEGK"
    expected = Right <$> outputs
    actual = liftM fromValidChar . applyPlugboard (plugboard pgSettings) . fromChar <$> inputs
