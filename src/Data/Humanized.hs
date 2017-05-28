module Data.Humanized
    ( Humanized
    , mkHumanizedText
    , fromHumanizedText
    )  where

import           Data.Maybe (fromMaybe)
import qualified Data.Text  as T

type Humanized a = Maybe a

mkHumanizedText :: T.Text -> Humanized T.Text
mkHumanizedText s
    | T.null s   = Nothing
    | otherwise  = Just s

fromHumanizedText :: Humanized T.Text -> T.Text
fromHumanizedText = fromMaybe (T.pack "")
