import Data.Char (digitToInt, isDigit) 
import Data.Foldable (foldlM)

-- 1.
-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the section called “Explicit recursion”.

-- file: ch04/ch04.exercises.hs
-- asInt_fold :: String -> Int
-- 12 comments
-- Your function should behave as follows. 37 comments
--
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798

asInt_fold :: String -> Int
asInt_fold xs@(x:xs') = case x of
                          '-' -> -1 * (foldl f 0 xs')
                          _   -> foldl f 0 xs
                        where f acc x = acc * 10 + (digitToInt x)

-- Extend your function to handle the following kinds of exceptional conditions by calling error. 4 comments
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374
asInt_fold' :: String -> Int
asInt_fold' xs@[] = 0
asInt_fold' xs@(x:xs') = case x of
                           '-' -> (-1) * (foldl f 0 xs')
                           _   -> foldl f 0 xs
                         where f acc x = case isDigit x of
                                           False -> error ("Char.digitToInt: not a digit '" ++ [x] ++ "'")
                                           _     -> acc * 10 + (digitToInt x)


-- 2.
--
-- The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem. 2 comments
--
-- -- file: ch04/ch04.exercises.hs
-- type ErrorMessage = String
-- asInt_either :: String -> Either ErrorMessage Int
-- 21 comments
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'o'"
-- 22 comments
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either xs@[] = Right 0
asInt_either xs@(x:xs') = case x of
                           '-' -> (foldlM f 0 xs') >>= (\acc -> Right (acc * (-1)))
                           _   -> foldlM f 0 xs
                         where f acc x = case isDigit x of
                                           False -> Left ("Char.digitToInt: not a digit '" ++ [x] ++ "'")
                                           _     -> Right (acc * 10 + (digitToInt x))

-- 3.
--
-- The Prelude function concat concatenates a list of lists into a single list, and has the following type. No comments
--
-- -- file: ch04/ch04.exercises.hs
-- concat :: [[a]] -> [a]
-- 9 comments
-- Write your own definition of concat using foldr. 20 comments

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs


-- 4.
--
-- Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr. 46 comments
takeWhileR _ []           = []
takeWhileR p xs@(x:xs')
   | p x                  = x : takeWhileR p xs'
   | otherwise            = []

takeWhileFoldl p xs       = fst $ foldl f ([],True) xs
                              where f (acc, False) x = (acc, False)
                                    f (acc, True) x  = case p x of
                                                         False -> (acc, False)
                                                         _     -> (acc ++ [x], True)


-- 5.
--
-- The Data.List module defines a function, groupBy, which has the following type. 9 comments
--
-- -- file: ch04/ch04.exercises.hs
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- No comments
-- Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold. 61 comments
groupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldl eq xs = foldr f [] xs
                     where f x []          = [[x]]
                           f x ((y:ys):zs) 
                             | eq x y      = ((x:y:ys):zs)
                             | otherwise   = ([x]:(y:ys):zs)
--
-- 6.
--
-- How many of the following Prelude functions can you rewrite using list folds? 2 comments
--
-- any 16 comments
--
-- cycle 19 comments
--
-- words 27 comments
--
-- unlines 11 comments
--
-- For those functions where you can use either foldl' or foldr, which is more appropriate in each case? 18 comments
--

main = do
  print $ "101"
  print $ asInt_fold "101"
  print $ "-31337"
  print $ asInt_fold "-31337"
  print $ "1798"
  print $ asInt_fold "1798"
  print $ "0"
  print $ asInt_fold' ""
  print $ "0"
  print $ asInt_fold' "-"
  print $ "-3"
  print $ asInt_fold' "-3"
--  print $ "2.7 : Exception: Char.digitToInt: not a digit '.' "
--  print $ asInt_fold' "2.7"
  print $ "564616105916946374"
  print $ asInt_fold' "314159265358979323846"
  print $ "2.7 : error message : Char.digitToInt: not a digit '.' "
  print $ asInt_either "2.7"
  print $ "abcdefg"
  print $ concat' ["abc","d","e","fg"]
  print $ "123"
  print $ takeWhileR (<= 3) [1,2,3,4,5,6,7]
  print $ "123"
  print $ takeWhileFoldl (<= 3) [1,2,3,4,5,6,7]
  print $ "[1,1], [2,2], [3,3,3], [5,5,5], [6,6]"
  print $ groupByFoldl (==) [1,1,2,2,3,3,3,5,5,5,6,6]

