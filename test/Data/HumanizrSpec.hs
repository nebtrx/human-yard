{-# LANGUAGE OverloadedStrings #-}
module Data.HumanizrSpec
    ( spec
    ) where

import           Control.Monad  (mapM_)
import           Data.Humanized
import           Data.Humanizr
import qualified Data.Text      as T
import           Test.Hspec

spec :: Spec
spec = do
    describe "Humanize Strings" $ do
        it "Can humanize string in pascal case" $ do
            let expectationsExample = [ ("PascalCaseInputStringIsTurnedIntoSentence", "Pascal case input string is turned into sentence")
                                      , ("WhenIUseAnInputAHere", "When I use an input a here")
                                      , ("10IsInTheBegining", "10 is in the begining")
                                      , ("NumberIsFollowedByLowerCase5th", "Number is followed by lower case 5th")
                                      , ("NumberIsAtTheEnd100", "Number is at the end 100")
                                      , ("XIsFirstWordInTheSentence", "X is first word in the sentence")
                                      , ("XIsFirstWordInTheSentence ThenThereIsASpace", "X is first word in the sentence then there is a space")
                                      , ("ContainsSpecial?)@Characters", "Contains special characters")
                                      , ("a", "A")
                                      , ("A", "A")
                                      , ("?)@", "")
                                      , ("?", "")
                                      , ("", "")
                                      , ("JeNeParlePasFrançais", "Je ne parle pas français")
                                      ]
            mapM_ humanizeExpectation expectationsExample

        it "Can humanize string with underscores and dashes" $ do
            let expectationsExample = [ ("Underscored_input_string_is_turned_into_sentence", "Underscored input string is turned into sentence")
                                      , ("Underscored_input_String_is_turned_INTO_sentence", "Underscored input String is turned INTO sentence")
                                      , ("TEST 1 - THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1 -THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1- THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1_ THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1 _THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1 _ THIS IS A TEST", "TEST 1 THIS IS A TEST")
                                      , ("TEST 1 - THIS_IS_A_TEST", "EST 1 THIS IS A TEST")
                                      , ("TEST 1 - THIS is A Test", "TEST 1 THIS is A test")
                                      ]
            mapM_ humanizeExpectation expectationsExample

        it "Can humanize string with acronyms" $ do
            let expectationsExample = [ ("HTML", "HTML")
                                      , ("TheHTMLLanguage", "The HTML language")
                                      , ("HTMLIsTheLanguage", "HTML is the language")
                                      , ("TheLanguage IsHTML", "The language is HTML")
                                      , ("TheLanguageIsHTML", "The language is HTML")
                                      , ("HTML5", "HTML 5")
                                      , ("1HTML", "1 HTML")
                                      ]
            mapM_ humanizeExpectation expectationsExample

humanizeExpectation :: (T.Text, T.Text) -> Expectation
humanizeExpectation (text, expectation) = humanize text `shouldBe` expectation
