-- file: ch08/GlobRegexEither.hs
module GlobRegexEither
  (
    globToRegex
  , matchesGlob
  ) where

import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String

-- file: ch08/GlobRegexEither.hs
--globToRegex cs = '^' : globToRegex' cs ++ "$"
globToRegex cs = eitherRight ("^" ++) $ eitherRight(++ "$") $ globToRegex' cs

-- file: ch08/GlobRegexEither.hs
globToRegex' :: String -> Either GlobError String

globToRegex' "" = Right ""

globToRegex' ('*':cs) = eitherRight (".*" ++) $ globToRegex' cs

globToRegex' ('?':cs) = eitherRight ("." ++) $ globToRegex' cs

globToRegex' ('[':'!':c:cs) = eitherRight ( ("[^" ++ [c]) ++) $ charClass cs
globToRegex' ('[':c:cs)     = eitherRight (("[" ++ [c]) ++) $ charClass cs
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs) = eitherRight (escape c ++) $ globToRegex' cs

-- file: ch08/GlobRegex.hs
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

-- file: ch08/GlobRegex.hs
charClass :: String -> Either GlobError String
charClass (']':cs) = eitherRight ("]" ++) $ globToRegex' cs
charClass (c:cs)   = eitherRight ([c] ++) $ charClass cs
charClass []       = Left "unterminated character class"

-- file: ch08/GlobRegex.hs
matchesGlob :: FilePath -> String -> Either GlobError Bool
--name `matchesGlob` pat = name =~ globToRegex pat
name `matchesGlob` pat =
  case globToRegex pat of
    Left err -> Left err
    Right pat' -> Right (name =~ pat')

eitherRight :: (a -> b) -> Either c a -> Either c b
eitherRight f = either Left (Right . f)

-- main = 
--   print $ globToRegex "aa[!kkk]*"
