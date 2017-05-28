{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Humanizr
    where

import           Data.Char                 (isUpper, toLower, toUpper)
import           Data.Char                 (isUpper)
import           Data.Foldable             (foldr, null)
import           Data.Humanized
import           Data.Humanizr.Transformer
import           Data.List                 (concat, intercalate, intersperse)
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import           Data.Tuple                (fst)
import           Text.Regex.PCRE.Heavy

import qualified Data.Text.Utils           as TU

-- TODO: Use Convertible Strings API


pascalCaseWordPartsRegex = [re|[\p{Lu}]?[\p{Ll}]+|[0-9]+[\p{Ll}]*|[\p{Lu}]+(?=[\p{Lu}][\p{Ll}]|[0-9]|\b)|[\p{Lo}]+|]
freestandingSpacingCharRegex = [re|\s[-_]|[-_]\s|]

-- Utilitites ^^ refactor in another module and add tests

fromUnderscoreDashSeparatedWords :: T.Text -> T.Text
fromUnderscoreDashSeparatedWords = T.intercalate "" . TU.splitBy ['-', '_']

fromPascalCase :: T.Text -> T.Text
fromPascalCase = ensureFirstCase . T.unwords . parsePascalCaseWordParts
    where
        parsePascalCaseWordParts :: T.Text -> [T.Text]
        parsePascalCaseWordParts s = casefy . fst <$> matches
            where matches = scan pascalCaseWordPartsRegex s :: [(T.Text, [T.Text])]

        casefy :: T.Text -> T.Text
        casefy match = if TU.isUpper match && ( T.length match > 1 || match == "I")
            then match
            else T.toLower match

        ensureFirstCase :: T.Text -> T.Text
        ensureFirstCase r = if not . T.null $ r
            then toUpper (T.head r) `T.cons` TU.safeTail r
            else r

humanize :: T.Text -> T.Text
humanize s = if TU.isUpper s
    then
        ( if s =~ freestandingSpacingCharRegex
              then fromPascalCase . fromUnderscoreDashSeparatedWords $ s
              else
                  ( if '_' `TU.elem` s || '-' `TU.elem` s
                        then fromUnderscoreDashSeparatedWords s
                        else fromPascalCase s
                  )
        )
    else fromPascalCase s

humanizedWithCase :: T.Text -> Transformer -> T.Text
humanizedWithCase i t = fromHumanizedText . (-/> t) . mkHumanizedText $ i

infixl 4 .-/>
(.-/>) :: T.Text -> Transformer -> T.Text
(.-/>) = humanizedWithCase
