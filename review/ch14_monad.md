# Chapter 14 Monad
Chapter 7에서 IO monad에 대해서 봤었다.  
하지만, IO monad를 바깥세상과 communicate하는 도구로써만 한정적으로만 보았었다. monad가 무엇인지는 아직 말하지 않았다.  

Chapter 7에서 IO monad는 다루는 것은 어렵지 않았었다.  
Notation이 좀 달랐었는데, imperative language에서의 코드와 크게 다르지 않았다.    

앞 장들에서 실제적으로 만난 문제들을 해결하는데 사용한 structure가 사실 monad이다.    
이 장에서 monad가 어떤 문제를 해결함에 있어서 명백하고, 유용한 툴로 자주 쓰인다는 것을 보이고,  
몇몇 monad를 정의함으로써, monad를 정의하는 것이 얼마나 쉬운가 보이겠다. 


### Revisiting Earlier Code Examples

##### Maybe Chaining

* 10 장의 코드 ParseP5  

```hs
-- file: ch10/PNM.hs
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)

parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)

```

* 오른쪽으로 심하게 치우치는 코드 개선  
   
```hs
-- file: ch10/PNM.hs
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

```

##### Implicit State
(>>?) 를 사용함으로써 오른쪽으로 치우치는 코드가 개선되고, 파싱한 결과와, 남은 stream을 pair로 다음으로 넘기도록 강제하게되었다.  

```hs
-- file: ch10/PNM.hs
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s       >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)
```

또 다시 발견되는 공통되는 패턴 : 스트림에서 일부 string을 소비하고, 결과와 남은 스트림을 반환한다.
만약 여기서 우리가 남은 stream 외에 한 가지를 더 다음 chain으로 넘기기를 원한다면? : 모든 스텝의 코드가 수정되어야 할 것이다.
상태를 다음 chain으로 넘기는 코드를 개개 스텝의 함수에서 chain함수로 넘기는 것이 좋겠다.

상태를 넘기는 코드를 chain함수로 넘긴 것.

```hs
-- file: ch10/Parse.hs
(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
```

parsing state의 상세도 숨겼고, getState, putState 함수들도 parsing state의 상세함을 몰라도 이용할 수 있으므로,
(어떻게 숨겼는지 앞 챕터를 봐야함. 여기서는 패턴매칭등이 사용되지 않고, 실제 Data Constructor를 몰라도 이용할 수 있다 정도로 이해하면 됨)
ParseSate가 변경되더라도 코드가 많이 변경될 필요는 없다.

### Looking for Shared Patterns
위에서 본 것이 그다지 일반적으로 보이지 않을 수 있다.
우리 코드를 간략하게 만들 수 있도록, 두 가지(Maybe, Parse) 코드 모두 함수를 chaining 하고 그 상세를 감춰주는데 신경을 쓰고있다.  

다시 그 정의부터 다시 봐 보자.
왼쪽에 한 개의 타입파라미터가 오고, 오른쪽에 그 파라미터가 사용되고 있다.
```hs
-- file: ch14/Maybe.hs
data Maybe a = Nothing
             | Just a
```

```hs
-- file: ch10/Parse.hs
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
```

chaining 함수의 타입을 보자. 비슷하다.

```hs
ghci> :type (>>?)
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
```
```hs
ghci> :type (==>)
(==>) :: Parse a -> (a -> Parse b) -> Parse b
``` 
위 둘을 아래와 같이 하나로 만들 수 있겠다.
```hs
-- file: ch14/Maybe.hs
chain :: m a -> (a -> m b) -> m b
```

끝으로, 각각의 케이스에 "plain" value를 받아서 그것을 타겟타입에 "injects" 하는 함수가 필요하다.
Maybe의 경우로 보자면, 단순히 Just일 것이다. 아래는 Parse의 경우 예이다.
```hs
-- file: ch10/Parse.hs
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
```

Maybe, Parse의 inject 함수를 공통적으로 보자면 아래와 같이 생겼다.
```hs
-- file: ch14/Maybe.hs
inject :: a -> m a
```

**3가지 속성**
* A type constructor m
* chaining 함수 m a -> (a -> m b) -> m b
* injection function a -> m a

### The Monad Typeclass
prelude의 Monad 정의
```hs
-- file: ch14/Maybe.hs
class Monad m where
    -- chain , bind라고도 함.
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a
```
* (>>)
	* (>>=) 와 달리 결과를 다음 chain 함수의 입력으로 넘기지 않는다. 결과가 필요없는 경우 유용.
	* (>>=) 를 이용해서 구현 함.
```hs
ghci> print "baz" >> print "quux"
"baz"
"quux"
```
* fail
	* monad의 핵심함수는 아님
	* 디폴트 구현은 error
```hs
-- file: ch14/Maybe.hs
    fail :: String -> m a
    fail = error
```
* Parse의 Monad instance 구현을 다시보자
```hs
-- file: ch10/Parse.hs
instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail
```

### And Now, a Jargon Moment
* Monadic
	* monad와 관련있다.
	* monadic type 이라고 하면 Monad typeclass의 instance라는 거다.
* "is a monad"
	*  Monad typeclass의 instance라는 거다.
	*  type constructor, injection function(return), chaining function (>>=, bind) 를 정의해야 한다. 
* "the Foo monad"
	*  Foo가 monad 의 instance라는거다
* action
	* monadic value의 다른 이름
	* IO monad에서 유래한 것 같다.
	* print "foo" 와 같은 monadic value는 side effect를 가진다.
	* monadic value를 리턴하는 함수는 action이라고 불린다.

### Using a New Monad: Show Your Work!
기존에 보았던 코드가 사실 monad 였다는 것을 보았다. 이제 Monad를 만들어 보자.  
Pure Haskell 코드는 깔끔하지만, 이것만으로는 I/O를 할 수가 없다.
이전에 보았던 globToRegex 함수를 로그를 남기도록 변환해 보자.

Logger로 감싸는 것으로 시작해 보자.
```hs
-- file: ch14/Logger.hs
globToRegex :: String -> Logger String
```

##### Information Hiding
* Logger의 내부는 감추자
	* 구현 변경에 있어 자유도를 가질 수 있고
	* 더 중요하게는, 단순한 interface를 보여준다.
```hs
-- file: ch14/Logger.hs
module Logger
    (
      Logger     -- type constructor만 노출하고, value constructor는 감춘다.
    , Log        -- string list이다. type Log = [String] 
    , runLogger  -- :: Logger a -> (a, Log)
    , record
    ) where
```
* runLogger
	* value constructor를 노출하지 않는 대신 Logger에 있는 결과와 그 결과가 나오기 까지 남은 log를 가져올 수 있는 함수를 제공한다.

##### Controlled Escape


##### Leaving a Trace


##### Using the Logger Monad


### Mixing Pure and Monadic Code 
Not yet