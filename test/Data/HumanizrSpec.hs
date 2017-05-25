module Data.HumanizrSpec
    ( spec
    ) where

import           Data.Humanizr
import           Test.Hspec

spec :: Spec
spec = do
    describe "Humanize Strings" $ do
        it "Can humanize string in pascal case" $ do
            humanize "PascalCaseInputStringIsTurnedIntoSentence" `shouldBe` "Pascal case input string is turned into sentence"
            humanize "WhenIUseAnInputAHere" `shouldBe` "When I use an input a here"
            humanize "10IsInTheBegining" `shouldBe` "10 is in the begining"
            humanize "NumberIsFollowedByLowerCase5th" `shouldBe` "Number is followed by lower case 5th"
            humanize "NumberIsAtTheEnd100" `shouldBe` "Number is at the end 100"
            humanize "XIsFirstWordInTheSentence" `shouldBe` "X is first word in the sentence"
            humanize "XIsFirstWordInTheSentence ThenThereIsASpace" `shouldBe` "X is first word in the sentence then there is a space"
            humanize "ContainsSpecial?)@Characters" `shouldBe` "Contains special characters"
            humanize "a" `shouldBe` "A"
            humanize "A" `shouldBe` "A"
            humanize "?)@" `shouldBe` ""
            humanize "?" `shouldBe` ""
            humanize "" `shouldBe` ""
            humanize "JeNeParlePasFrançais" `shouldBe` "Je ne parle pas français"

        it "Can humanize string with underscores and dashes" $ do
            humanize "Underscored_input_string_is_turned_into_sentence" `shouldBe` "Underscored input string is turned into sentence"
            humanize "Underscored_input_String_is_turned_INTO_sentence" `shouldBe` "Underscored input String is turned INTO sentence"
            humanize "TEST 1 - THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1 -THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1- THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1_ THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1 _THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1 _ THIS IS A TEST" `shouldBe` "TEST 1 THIS IS A TEST"
            humanize "TEST 1 - THIS_IS_A_TEST" `shouldBe` "EST 1 THIS IS A TEST"
            humanize "TEST 1 - THIS is A Test" `shouldBe` "TEST 1 THIS is A test"




-- [InlineData("Underscored_input_string_is_turned_into_sentence", "Underscored input string is turned into sentence")]
--         [InlineData("Underscored_input_String_is_turned_INTO_sentence", "Underscored input String is turned INTO sentence")]
--         [InlineData("TEST 1 - THIS is A Test", "TEST 1 THIS is A test")]
