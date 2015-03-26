import Data.List
import Data.Function
import Data.Ord
import Tree

--  1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function. 49 comments
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

--  2.
--  Add a type signature for your function to your source file. To test it, load the source file into ghci again.

--  3.
--  Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)
mean' :: Fractional a => [a] -> a
mean' xs = (/) (sum' xs) (fromIntegral $ length' xs)
  where sum' (x:xs) = x + sum' xs
        sum' [] = 0

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
  where reverse'' (x:xs) ys = reverse'' xs (x:ys)
        reverse'' []  ys = ys
--  4.
--  Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]. 71 comments
palindrome :: [a] -> [a]
palindrome [] = []
palindrome [x] = [x]
palindrome (x:xs) = xs ++ (reverse' xs)

--  5.
--  Write a function that determines whether its input list is a palindrome. 85 comments
is_palindrome :: (Eq a) => [a] -> Bool
is_palindrome xs = xs == (reverse' xs)

--  6.
--  Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.) 43 comments
sortList :: [[a]] -> [[a]]
sortList = sortBy (comparing length)

--  7.
--  Define a function that joins a list of lists together using a separator value. 25 comments
--  -- file: ch03/Intersperse.hs
intersperse' :: a -> [[a]] -> [a]
intersperse' a [] = []
intersperse' a [x] = x
intersperse' a (x:xs) = x ++ (a:(intersperse' a xs))

--  8.
--  Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on. 34 comments
height :: Tree a -> Int
height Empty = 0
height (Node n l r) = 1 + (max (height l) (height r))

--  9.
--  Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities. 24 comments
data Direction = DirectionLeft | DirectionRight | DirectionStraight
data Point = Point {
  x :: Double,
  y :: Double
} deriving (Show)

--  10.
--  Write a function that calculates the turn made by three 2D points and returns a Direction. 58 comments
calcDirection :: Point -> Point -> Point -> Direction
calcDirection a b c = DirectionLeft



--  11.
--  Define a function that takes a list of 2D points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction. 19 comments


--  12.
--  
--  Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia. 61 comments


main = do
  print $ 0
  print $ length' []
  print $ 1
  print $ length' [1]
  print $ 100
  print $ length' [1,2..100]
  print $ "mean [1..100]"
  print $ mean' [1..100]
  print $ "palindrome [1,2..5]"
  print $ palindrome [1,2..5]
  print $ "False"
  print $ is_palindrome [1,2..5]
  print $ "True"
  print $ is_palindrome $ palindrome [1,2..5]
  print $ "sortList [[1,2..30],[1,2..10],[1,2..4]]"
  print $ sortList [[1,2..30],[1,2..10],[1,2..4]]
  print $ "intersperse ',' [foo,bar,baz,quux]"
  print $ intersperse' ',' ["foo","bar","baz","quux"]
  print $ 1
  print $ height (Node "x" Empty Empty)
  print $ 3
  print $ height (Node "x" (Node "l" Empty Empty) (Node "r" Empty (Node "rr" Empty Empty)))

