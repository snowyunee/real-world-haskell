
-- 4.
-- Write a program that transposes the text in a file.
-- For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-- e1_4.hs


import System.Environment (getArgs)
import Data.List (transpose)

transpose' :: [[a]] -> [[a]]
transpose' []           = []
transpose' ([]:xss)     = transpose' xss
transpose' ((x:xs):xss) = (x : [h | (h:_) <- xss]) : transpose' (xs : [t | (_:t) <- xss])


printWith function inputFile = do
  input <- readFile inputFile
  putStrLn (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> printWith function input
            _ -> putStrLn "usage: e1_4 input_file_name"

        myFunction xs = unlines $ transpose' $ lines xs
