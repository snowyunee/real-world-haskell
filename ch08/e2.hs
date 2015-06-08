-- Exercises
-- 1.
-- Although we've gone to some lengths to write a portable namesMatching function,
-- the function uses our case sensitive globToRegex function.
-- Find a way to modify namesMatching to be case sensitive on Unix,
-- and case insensitive on Windows, without modifying its type signature. 12 comments
--
-- Hint: consider reading the documentation for System.FilePath to look for a variable that tells us whether we're running on a Unix-like system, or on Windows. 3 comments


-- 2.
-- If you're on a Unix-like system, look through the documentation for the System.
-- Posix.Files module, and see if you can find a replacement for the doesNameExist function. 1 comment
-- : fileExist : this words both file and directory


-- 3.
-- The * wild card only matches names within a single directory.
-- Many shells have an extended wild card syntax, **, that matches names recursively in all directories.
-- For example, **.c would mean “match a name ending in .c in this directory or any subdirectory at any depth”.
-- Implement matching on ** wildcards. 7 comments



main = do
  print $ 
