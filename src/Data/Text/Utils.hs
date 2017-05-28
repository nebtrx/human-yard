module Data.Text.Utils
    where
import qualified Data.Char  as CH
import           Data.Maybe (isJust)
import qualified Data.Text  as T
import           Prelude    (Bool (..), Char (..), Maybe (..), otherwise, ($),
                             (.), (==))
import qualified Prelude    as P

splitBy :: [Char] -> T.Text -> [T.Text]
splitBy cs = T.split (`P.elem` cs)

isUpper :: T.Text -> Bool
isUpper = T.all CH.isUpper

elem :: Char -> T.Text -> Bool
elem c = isJust . T.find (== c)

safeTail :: T.Text -> T.Text
safeTail s
  | T.null s   = T.pack ""
  | otherwise  = T.tail s

safeHead :: T.Text -> Maybe Char
safeHead s
    | T.null s  = Nothing
    | otherwise = Just $ T.head s
