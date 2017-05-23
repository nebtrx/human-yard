{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.Char             (toLower, toUpper)
import           Data.Char             (isUpper)
import           Data.Foldable         (foldr, null)
import           Data.Tuple            (fst)
import           Text.Regex.PCRE.Heavy


------------------------------------------------------------------------------
{- | Transforms first element of the 'String' using the function given. -}
-- transformFst :: (Char -> Char) -> String -> String
-- transformFst _   ""   = ""
-- transformFst f (x:xs) = f x:xs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts CamelCase or mixedCase 'String' to snake_string.
     POSIX Regex approach.
     Due to the POSIX Regex syntax limitations, this still needs an extra
     case because the first character of the result could be an underscore.
-}
-- toSnake :: String -> String
-- toSnake = downcase . go
--   where
--     downcase = map toLower
--     go s = case subRegex (mkRegex "[A-Z]") s "_\\0" of
--       ('_':xs) -> xs
--       x        -> x

-- toDash :: String -> String
-- toDash = downcase . go
--   where
--     downcase = map toLower
--     go s = case subRegex (mkRegex "[A-Z]") s "-\\0" of
--       ('-':xs) -> xs
--       x        -> x
-- ------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts snake_string to mixedCase 'String'.
     Uses a custom simple subRegex variant, which accepts a transforming
     function as an argument, and applies it to all recursively found
     matches in a 'String' given.
     NOTE: The regular expression used as a matcher will not match a
     trailing \"_\" characters, so a 'String' like "\foo_\" will result in
     a \"Foo_\" return value.
-}
-- toMixed :: String -> String
-- toMixed = subRegex' (mkRegex "_[a-zA-Z]+") go
--   where
--     subRegex' :: Regex -> (String -> String) -> String -> String
--     subRegex' r n s = case matchRegexAll r s of
--       Nothing           -> s
--       Just (b, m, a, _) -> b ++ n m ++ subRegex' r n a

--     go :: String -> String
--     go []     = []
--     go (x:xs) = transformFst toUpper xs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
{- | Converts snake_string to CamelCase 'String'.
     Actually it is a combinator which upcases the first letter of a
     'String', previously converted to mixedCase.
-}
-- toCamel :: String -> String
-- toCamel = transformFst toUpper . toMixed
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- taken from https://gist.github.com/ruthenium/3730291

-- pcre-heavy
pascalCaseWordPartsRegex = [re|[A-Z][a-z]+|[A-Z]+(?![a-z])|]
freestandingSpacingCharRegex = [re|\s[-_]|[-_]\s|]

parsePascalCaseWordParts :: String -> [String]
parsePascalCaseWordParts s = reverse $ fst <$> (scan pascalCaseWordPartsRegex s :: [(String, [String])])

splitBy :: [Char] -> String -> [String]
splitBy cs s =  case dropWhile predicate s of
    "" -> []
    s' -> w : splitBy cs s''
        where (w, s'') = break predicate s'
    where
        predicate = flip elem cs

fromUnderscoreDashSeparatedWords :: String -> String
fromUnderscoreDashSeparatedWords = foldr joinFn "" . splitBy ['-', '_']
    where joinFn e acc =  acc ++ " " ++ e

fromPascalCase :: String -> String
fromPascalCase = foldr joinFn "" . parsePascalCaseWordParts
    where
        joinFn e acc =  accAndSeparator acc ++ e
        accAndSeparator acc = if null acc
            -- FIXME: i don't like this, probably a map(not trivial)
            then acc
            else acc ++ " "

humanize :: String -> String
humanize s = if all isUpper s
    then
        ( if s =~ freestandingSpacingCharRegex
              then fromPascalCase . fromUnderscoreDashSeparatedWords $ s
              else
                  ( if '_' `elem` s || '-' `elem` s
                        then fromUnderscoreDashSeparatedWords s
                        else fromPascalCase s
                  )
        )
    else fromPascalCase s




-- // if input is all capitals (e.g. an acronym) then return it without change
--             if (input.ToCharArray().All(char.IsUpper))
--                 return input;

--             // if input contains a dash or underscore which preceeds or follows a space (or both, e.g. free-standing)
--             // remove the dash/underscore and run it through FromPascalCase
--             if (FreestandingSpacingCharRegex.IsMatch(input))
--                 return FromPascalCase(FromUnderscoreDashSeparatedWords(input));

--             if (input.Contains("_") || input.Contains("-"))
--                 return FromUnderscoreDashSeparatedWords(input);

--             return FromPascalCase(input);

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
main = print "Hello World"
  -- print . toCamel $ "was_snake_string"
  -- print . toCamel $ "was-dash-string"

  -- print . toSnake $ "HTTPWasCamelCase"
  -- print . toSnake $ "wasHTTPMixedCase"

  -- print . toDash $ "wasMixedCase"
