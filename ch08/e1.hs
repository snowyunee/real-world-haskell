--Exercises

--1.
--Use ghci to explore what happens if you pass a malformed pattern, such as [, to globToRegex. Write a small function that calls globToRegex, and pass it a malformed pattern. What happens? 5 comments
-- bRegex> "foo.c" =~ globToRegex "[" :: Bool
-- Loading package array-0.4.0.1 ... linking ... done.
-- Loading package deepseq-1.3.0.1 ... linking ... done.
-- Loading package bytestring-0.10.0.2 ... linking ... done.
-- Loading package containers-0.5.0.0 ... linking ... done.
-- Loading package transformers-0.3.0.0 ... linking ... done.
-- Loading package mtl-2.1.2 ... linking ... done.
-- Loading package regex-base-0.93.2 ... linking ... done.
-- Loading package regex-posix-0.95.2 ... linking ... done.
-- *** Exception: unterminated character class


--2.
--While filesystems on Unix are usually sensitive to case (e.g. “G” vs. “g”) in file names, Windows filesystems are not.
--Add a parameter to the globToRegex and matchesGlob functions that allows control over case sensitive matching. 8 comments

import GlobRegex
import Text.Regex.Posix ((=~))
import System.FilePath (pathSeparator)


isWindows :: Bool
isWindows = pathSeparator == '\\'

main = do
  print $ "False"
  print $ matchesGlob "KKK" "kkk"
  print $ "True"
  print $ matchesGlob' "KKK" isWindows "kkk"
  print $ "False"
  print $ matchesGlob' "KKK" True "kkk"
