-- file: ch08/GlobRegex.hs
module GlobRegex
    (
      globToRegex
    , matchesGlob
    , matchesGlob'
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toLower)

globToRegex :: String -> String

-- file: ch08/GlobRegex.hs
globToRegex cs = '^' : globToRegex' cs ++ "$"

-- file: ch08/GlobRegex.hs
globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

-- file: ch08/GlobRegex.hs
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

-- file: ch08/GlobRegex.hs
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

-- file: ch08/GlobRegex.hs
matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

-- file: ch08/GlobRegex.hs
--globToRegex' (c:cs) = escape c ++ globToRegex' cs

matchesGlob' :: FilePath -> Bool -> String -> Bool
matchesGlob' pat True name  = name =~ globToRegex pat
matchesGlob' pat False name = (map toLower name) =~ globToRegex (map toLower pat)
