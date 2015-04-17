-- 3.
-- Using the command framework from the section called “A simple command line framework”,
-- write a program that prints the first word of each line of its input.
-- e1_3.hs

import System.Environment (getArgs)

printWith function inputFile = do
  input <- readFile inputFile
  putStrLn (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> printWith function input
            _ -> putStrLn "usage: e1_3 input_file_name"

        myFunction xs = unlines $ map head $ map words $ lines xs
