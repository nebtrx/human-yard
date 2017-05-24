{-# LANGUAGE QuasiQuotes #-}

module Data.Humanizr
    where

import           Data.Char             (toLower, toUpper)
import           Data.Char             (isUpper)
import           Data.Foldable         (foldr, null)
import           Data.Tuple            (fst)
import           Text.Regex.PCRE.Heavy

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

