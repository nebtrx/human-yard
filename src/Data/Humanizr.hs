{-# LANGUAGE QuasiQuotes #-}

module Data.Humanizr
    where

import           Data.Char             (toLower, toUpper)
import           Data.Char             (isUpper)
import           Data.Foldable         (foldr, null)
import           Data.List             (concat, intersperse)
import           Data.Tuple            (fst)
import           Text.Regex.PCRE.Heavy

pascalCaseWordPartsRegex = [re|[\p{Lu}]?[\p{Ll}]+|[0-9]+[\p{Ll}]*|[\p{Lu}]+(?=[\p{Lu}][\p{Ll}]|[0-9]|\b)|[\p{Lo}]+|]
freestandingSpacingCharRegex = [re|\s[-_]|[-_]\s|]

splitBy :: [Char] -> String -> [String]
splitBy cs s =  case dropWhile predicate s of
    "" -> []
    s' -> w : splitBy cs s''
        where (w, s'') = break predicate s'
    where
        predicate = flip elem cs

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

isUpperString :: String -> Bool
isUpperString = all isUpper

fromUnderscoreDashSeparatedWords :: String -> String
fromUnderscoreDashSeparatedWords = join "" . splitBy ['-', '_']

fromPascalCase :: String -> String
fromPascalCase = ensureFirstCase . join " " . parsePascalCaseWordParts
    where
        parsePascalCaseWordParts :: String -> [String]
        parsePascalCaseWordParts s = casefy
            . fst <$> (scan pascalCaseWordPartsRegex s :: [(String, [String])])

        casefy :: String -> String
        casefy match = if isUpperString match && (length match > 1 || match == "I")
            then match
            else toLower <$> match

        ensureFirstCase :: String -> String
        ensureFirstCase r = if length r > 0
            then (toUpper <$> take 1 r) ++ drop 1 r
            else r

humanize :: String -> String
humanize s = if isUpperString s
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

