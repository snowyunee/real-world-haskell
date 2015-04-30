import SimpleJSON
import PrettyJSON
import Prettify

space = '_'

fill :: Int -> Doc -> Doc
fill width x = hcat $ best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> d : best (col + 1) ds
                Text s       -> d : best (col + length s) ds
                Line         -> Text (replicate (width - col) space) : d : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best col _ = [Text (replicate (width - col) space)]

          nicest col a b | (width - least) `fits2` a = a
                         | otherwise                 = b
                         where least = min width col

fits2 :: Int -> [Doc] -> Bool
w `fits2` _ | w < 0      = False
w `fits2` []             = True
w `fits2` (Empty:_)      = True
w `fits2` (Line:_)       = True
w `fits2` (Char x:xs)    = (w - 1) `fits2` xs
w `fits2` (Text x:xs)    = (w - length x) `fits2` xs


nest :: Int -> Doc -> Doc
nest width x = hcat $ best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char '{'     -> d : best (col + width) ds
                Char '['     -> d : best (col + width) ds
                Char ']'     -> d : best (col - width) ds
                Char '}'     -> d : best (col - width) ds
                Line         -> d : indent col : best col ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> best col (b:ds)
                _            -> d : best col ds
          best _ _ = [Empty]

          indent width = Text (replicate width ' ')

nest' :: Int -> Doc -> Doc
nest' width x = hcat $ best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char '{'     -> d : best (col + width) (Line:ds)
                Char '['     -> d : best (col + width) (Line:ds)
                Char ']'     -> d : best (col - width) ds
                Char '}'     -> d : best (col - width) ds
                Line         -> d : case head ds of
                                      Char ']' -> indent (col - width) : head ds : best (col- width) (tail ds)
                                      Char '}' -> indent (col - width) : head ds : best (col- width) (tail ds)
                                      _        -> indent col : best col ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> best col (b:ds)
                _            -> d : best col ds
          best _ _ = [Empty]

          indent width = Text (replicate width ' ')




jvalue = JObject [
           ("snow", JArray [
                            JString "snow 1", 
                            JString "snow 1", 
                            JNumber 12345,
                            JBool True,
                            JNull]),
           ("snow2", JArray [
                            JString "snow 2", 
                            JString "snow 3", 
                            JNumber 12345,
                            JBool True,
                            JNull]),
           ("snow3", JArray [
                            JString "snowwwwwwwwwwww", 
                            JString "snowwwwwwwwww", 
                            JNumber 12345,
                            JBool True,
                            JNull]),
           ("string", JString "string"),
           ("number", JNumber 12345),
           ("bool", JBool True)]


main = do
  putStrLn $ compact $ fill 10 $ renderJValue jvalue
  putStrLn $ compact $ nest' 5 $ renderJValue jvalue
  
