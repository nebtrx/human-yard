module Data.HumanizrSpec
    ( spec
    ) where

import           Data.Humanizr
import           Test.Hspec

spec :: Spec
spec = do
    describe "Humanize Strings" $ do
        it "CanHumanizeStringInPascalCase" $ do
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
