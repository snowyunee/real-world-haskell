-- 1.
-- Write your own “safe” definitions of the standard partial list functions,
-- but make sure that yours never fail.
-- As a hint, you might want to consider using the following types.
--
-- -- file: ch04/ch04.exercises.hs
-- safeHead :: [a] -> Maybe a
-- safeTail :: [a] -> Maybe [a]
-- safeLast :: [a] -> Maybe a
-- safeInit :: [a] -> Maybe [a]

safeHead :: [a] -> Maybe a
safeHead (x:_)   = Just x
safeHead []      = Nothing

safeTail :: [a] -> Maybe [a]
safeTail []      = Nothing
safeTail (_:xs)  = Just xs

safeLast :: [a] -> Maybe a
safeLast [x]     = Just x
safeLast (_:xs)  = safeLast xs
safeLast []      = Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs@[]          = Nothing
safeInit xs@(_:xs')     = Just (init' xs)
  where init' ys@(_:[])    = []
        init' ys@(y:ys')   = y : init' ys'

-- 2.
-- Write a function splitWith that acts similarly to words,
-- but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.
--
-- -- file: ch04/ch04.exercises.hs
-- splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ xs@[]      = [xs]
splitWith f xs@(x:xs') = cons (case break f xs of
                                 (ys, zs) -> (ys, case zs of
                                                   []      -> []
                                                   _:zs'   -> splitWith f zs'))
  where cons (a,b) = a:b
    

-- 3.
-- Using the command framework from the section called “A simple command line framework”,
-- write a program that prints the first word of each line of its input.
-- e1_3.hs

-- 4.
-- Write a program that transposes the text in a file. For instance,
-- it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-- e1_4.hs


main = do
  print "Nothing"
  print $ (safeHead [] :: Maybe Int)
  print "x"
  print $ safeHead "x"
  print "a"
  print $ safeHead "ab"
  print "Nothing"
  print $ (safeTail [] :: Maybe [Int])
  print "Just []"
  print $ safeTail "x"
  print "bc"
  print $ safeTail "abc"
  print "Nothing"
  print $ (safeLast [] :: Maybe Int)
  print "x"
  print $ safeLast "x"
  print "c"
  print $ safeLast "abc"
  print "Nothing"
  print $ (safeInit [] :: Maybe [Int])
  print "[]"
  print $ safeInit "x"
  print "ab"
  print $ safeInit "abc"

