-- 1.
--  1-1 return the last element of a list
--  1-2 cannot return for the infinite list


-- 2.
lastButOne :: [a] -> a
lastButOne (a:xs) = if islast xs then a else lastButOne xs

islast [a] = True
islast _ = False

-- 3.
-- *Main> lastButOne [1]
-- *** Exception: e.hs:8:1-58: Non-exhaustive patterns in function lastButOne
--


  
