-- 1. Write the converse of fromList for the List type: a function that takes a List a and generates a [a].

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons x xs) = x:(toList xs)
toList Nil       = []

-- 2. Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, use the Maybe type to refer to a node's children.
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

main = do
  print $ fromList "abcd"
  print $ toList (Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' Nil))))
  print $ Node "v" (Just (Node "L" Nothing Nothing)) Nothing
  print $ Node "v" (Just (Node "L" (Just (Node "LL" Nothing Nothing)) Nothing)) (Just (Node "R" Nothing Nothing))
  print $ Node "v" Nothing Nothing

