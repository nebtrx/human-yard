module Data.Humanizr.Transformer
    where

import           Data.Char       (isUpper, toLower, toUpper)
import           Data.Humanized
import           Data.Maybe      (Maybe (..), fromMaybe, isJust)
import qualified Data.Text       as T

import           Data.Text.Utils as TU

newtype Transformer = Transformer{ transformInput :: T.Text -> T.Text }

sentenceCaseTransformer :: Transformer
sentenceCaseTransformer = Transformer transFn
    where
        transFn input = if T.null input
            then T.pack ""
            else toUpper (T.head input) `T.cons` (TU.safeTail $ input)

titleCaseTransformer :: Transformer
titleCaseTransformer = Transformer transformFn
    where
        transformFn i = T.unwords $ (./> sentenceCaseTransformer) <$> T.words i

lowerCaseTransformer :: Transformer
lowerCaseTransformer = Transformer T.toLower

upperCaseTransformer :: Transformer
upperCaseTransformer = Transformer T.toUpper

applyTransformer :: T.Text -> Transformer -> T.Text
applyTransformer input tr = transformInput tr input

liftTransformer :: Humanized T.Text -> Transformer -> Humanized T.Text
liftTransformer input tr = (./> tr) <$> input

sequenceTransformers :: Transformer -> Transformer -> Transformer
sequenceTransformers (Transformer t1) (Transformer t2) = Transformer $ t2 . t1

-- TODO this can't be public
infixl 4 ./>
(./>) ::  T.Text -> Transformer -> T.Text
(./>) = applyTransformer

infixl 4 -/>
(-/>) :: Humanized T.Text -> Transformer -> Humanized T.Text
(-/>) = liftTransformer

infixl 4 ./
(./) ::  Transformer -> Transformer -> Transformer
(./) = sequenceTransformers

--"ffdfdfd" -./> casTr ./ upTR

--"ffdfdfd" .-/> casTr ./ upTR
