module Main where

import           Data.Char        (toLower, toUpper)
import           Text.Regex       (Regex, matchRegexAll, mkRegex, subRegex)
import           Text.Regex.Posix ((=~))


------------------------------------------------------------------------------
{- | Transforms first element of the 'String' using the function given. -}
transformFst :: (Char -> Char) -> String -> String
transformFst _   ""   = ""
transformFst f (x:xs) = f x:xs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts CamelCase or mixedCase 'String' to snake_string.
     POSIX Regex approach.
     Due to the POSIX Regex syntax limitations, this still needs an extra
     case because the first character of the result could be an underscore.
-}
toSnake :: String -> String
toSnake = downcase . go
  where
    downcase = map toLower
    go s = case subRegex (mkRegex "[A-Z]") s "_\\0" of
      ('_':xs) -> xs
      x        -> x

toDash :: String -> String
toDash = downcase . go
  where
    downcase = map toLower
    go s = case subRegex (mkRegex "[A-Z]") s "-\\0" of
      ('-':xs) -> xs
      x        -> x
------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts snake_string to mixedCase 'String'.
     Uses a custom simple subRegex variant, which accepts a transforming
     function as an argument, and applies it to all recursively found
     matches in a 'String' given.
     NOTE: The regular expression used as a matcher will not match a
     trailing \"_\" characters, so a 'String' like "\foo_\" will result in
     a \"Foo_\" return value.
-}
toMixed :: String -> String
toMixed = subRegex' (mkRegex "_[a-zA-Z]+") go
  where
    subRegex' :: Regex -> (String -> String) -> String -> String
    subRegex' r n s = case matchRegexAll r s of
      Nothing           -> s
      Just (b, m, a, _) -> b ++ n m ++ subRegex' r n a

    go :: String -> String
    go []     = []
    go (x:xs) = transformFst toUpper xs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts snake_string to CamelCase 'String'.
     Actually it is a combinator which upcases the first letter of a
     'String', previously converted to mixedCase.
-}
toCamel :: String -> String
toCamel = transformFst toUpper . toMixed
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- taken from https://gist.github.com/ruthenium/3730291

-- pcre-heavy
-- scan [re|[A-Z][a-z]+|[A-Z]+(?![a-z])|] "NumberOfABCDOmar":: [(String, [String])]

-- var rx = new Regex
--                 (@"([a-z]+[A-Z]|[A-Z][A-Z]+|[A-Z]|[^A-Za-z][^A-Za-z]+)");

--         string[] tests = {
--         "AutomaticTrackingSystem",
--         "XMLEditor",
--         "AnXMLAndXSLT2.0Tool",
--         "NumberOfABCDThings",
--         "AGoodMan",
--         "CodeOfAGoodMan"
--         };

main :: IO ()
main = do
  print . toMixed $ "was_snake_string"
  print . toCamel $ "was_snake_string"
  print . toCamel $ "was-dash-string"

  print . toSnake $ "HTTPWasCamelCase"
  print . toSnake $ "wasHTTPMixedCase"

  print . toDash $ "wasMixedCase"
