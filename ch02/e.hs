-- 1.
--  1-1 return the last element of a list
--  1-2 cannot return for the infinite list


-- 2.
lastButOne :: [a] -> a
lastButOne (a:xs) = if islast xs then a else lastButOne xs

islast [a] = True
islast _ = False

-- 3.
-- *Main> lastBugOne [1]
--
-- <interactive>:7:1:
--     Not in scope: ▒▒lastBugOne▒▒
--         Perhaps you meant ▒▒lastButOne▒▒ (line 8)
--


  
