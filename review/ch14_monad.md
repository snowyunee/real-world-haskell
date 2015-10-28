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

* 또 다시 발견되는 공통되는 패턴
	* 스트림에서 일부 string을 소비하고, 결과와 남은 스트림을 반환한다.  
* 만약 여기서 우리가 남은 stream 외에 한 가지를 더 다음 chain으로 넘기기를 원한다면?
	* 모든 스텝의 코드가 수정되어야 할 것이다. 
* 상태를 다음 chain으로 넘기는 코드를 개개 스텝의 함수에서 chain함수로 넘기는 것이 좋겠다.

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
이제 ParseSate가 변경되더라도 코드가 많이 변경될 필요는 없다.

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
monad typeclass는 monad 를 벗고(unwrap) plain value를 얻을 수 있는 어떤 방법을 제공하지 않는다.  
bind, return 모두 그에 해당하지 않는다. bind는 unwrap하지만, 다시 감싸서 반환해야 한다.  
대부분의 monads는 하나 이상의 runLogger와 같은 함수들을 제공한다.  
IO는 예외이다. 이는 프로그램 종료시점에 된다.


##### Leaving a Trace

* record
	* logger action안에서 실행된다.
	* 이러한 특별한 접근을 위한 untility들이 필요하다.


```hs
-- file: ch14/Logger.hs
record :: String -> Logger ()
``` 

```hs
ghci> let simple = return True :: Logger Bool
ghci> runLogger simple
(True,[])
```

```hs
ghci> runLogger (record "hi mom!" >> return 3.1337)
(3.1337,["hi mom!"])
```


##### Using the Logger Monad

```hs
-- file: ch14/Logger.hs
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)
```

### Mixing Pure and Monadic Code 
모나드에 상당한 단점이 있는것으로 볼 수 있다. Monad value는 일반함수와 섞어서 쓰는 것이 어렵다.     
예를 들어 로거 모나드에서 로깅한 string의 length를 바로 pure 한 length함수를 이용해서 알기는 어렵다.

```hs
ghci> let m = return "foo" :: Logger String
ghci> length m

<interactive>:1:7:
    Couldn't match expected type `[a]'
           against inferred type `Logger String'
    In the first argument of `length', namely `m'
    In the expression: length m
    In the definition of `it': it = length m
```
아래와 같이 해야 한다.
```hs
ghci> :type   m >>= \s -> return (length s)
m >>= \s -> return (length s) :: Logger Int
```

* lifting
	* "Introducing Functors" 에서 소개했었다.
	* function을 functor로 lifting 하는 것이다.
	* unwrap => function 호출 => 동일한 constructor로 rewrap 과정이라고 보면 된다.
	* Monad와 동일하다.
		* 모나드의 (>>=) : unwrap pure function 호출
		* return : rewrap
		* liftM은 Monad의 구체적인 구현을 알 필요 없이 (>>=)와 return을 이용하면 된다.

liftM 정의  
```hs
-- file: ch14/Logger.hs
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)
```

Functor의 instance인 타입을 정의하려면 fmap을 우리가 만들려는 타입에 맞게 정의해 주어야 하는 반면, liftM은 그럴 필요가 없다.    
(>>=)와 return에 추상화되어있으므로, 타입 정의할 때 한 번 정의하면 된다.    

liftM은 Control.Monad 모듈에 이미 정의되어있다.  
liftM 사용하지 않는 코드
```hs
-- file: ch14/Logger.hs
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return (c:ds)
```
liftM 사용 코드 (>>=과 보기 싫은 lambda 함수를 제거할 수 있었다.)
```hs
-- file: ch14/Logger.hs
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
```
fmap 처럼 liftM도 infix 형태로 자주 사용한다.  
의미는 왼쪽에 있는 pure 함수에 우측의 monadic action의 결과를 적용해라 이다.  

liftM이 굉장히 유용해서 여러 파라미터를 받는 버전이 Control.Monad에 정의되어 있다.
liftM2 사용와 liftM2 정의
```hs
-- file: ch14/Logger.hs
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

-- file: ch14/Logger.hs
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)
```
liftM5까지 정의되어있다.

### Putting a Few Misconceptions to Rest
자주 반복되는 monad에 대한 미신들
* Monad는 이해하기 어렵다.
	* 우리가 앞에서 몇몇 모나드를 보았는데, Monad는 몇몇 문제를 해결하기 위해 자연적으로 나오는 것이었다.  
	* Monad를 이해하는 가장 빠른 방법은 특정한 문제들을 살펴보고 그들을 해결하는데 공통점이 있는지 보는 것이다.
* Monad는 I/O와 imperative coding 에만 유용하다.
	* haskell I/O 이외의 영역에서 Monad가 쓰이는 것을 보았다. 
	* 함수 chaining을 짧게 하기 위해, 복잡한 state를 감추기 위해, logging을 위해 등등..
* Monad는 Haskell에만 있다.
	* Haskell이 monads를 가장 명시적으로 사용하는 언어이겠지만, 다른 언어에서도 사용하고 있다.  
	* 물론 Haskell에서 Monad를 가장 다루기 쉽다. do notation 때문에, 타입추론, language's syntax 등등..
* Monad는 evaluation 순서를 조절하기 위한 것이다. 


### Building the Logger Monad
단순한 로거 Monad를 만들어 보자.  

```hs
-- file: ch14/Logger.hs
newtype Logger a = Logger { execLogger :: (a, Log) }
```
단순한 pair이다. 여기서 a는 action의 result이고, Log는 a라는 action이 싱행되는 동안 발생한 로그 메시지의 리스트이다.  
다른 타입으로 만들기 위해 newtype으로 정의했다.  
runLogger는 그 tuple값을 Logger Monad로부터 꺼내는 작업을 한다.  
runLogger가 외부에 노출된 interface인데, 여기서는 execLogger의 다른 이름일 뿐이다.

record : 로그 메시지만 있다.

(>>=) : 이게 Monad의 핵심인데, action과 monadic function을 엮어서 새로운 결과와 결합된 새 로그를 만들어 준다.  

```hs
-- file: ch14/Logger.hs
runLogger = execLogger

record s = Logger ((), [s])

-- file: ch14/Logger.hs
instance Monad Logger where
    return a = Logger (a, [])

    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

```

##### Sequential Logging, Not Sequential Evaluation
위의 (>>=) 구현은 Logger b의 로그 메시지에, Logger a의 log가 먼저 오고, k a 실행 시 발생한 로그가 온다는 것은 보장하지만,  
a와 b가 어떤 순서로 실행되는지는 보장하지 않는다. (>>=) 는 lazy이다.

strictness라는 측면에서 monad의 동작은, monad의 다른 면들과 마찬가지로 각 monad의  구현이 어떻게 되어있느냐에 달려있는 것이지  
모든 monad들이 고정된 속성을 가지고 있는 것은 없다.    

##### The Writer Monad
우리가 위에서 작성한 Logger는 standard Writer monad(Control.Monad.Writer 모듈의)의 특별한 버전이다.  
"Using Typeclasses"에서 볼 예정이다. 


### The Maybe Monad
Maybe monad는 아마도 거의 가장 단순한 monad일 것 같다. procedure의 결과가 없을 수도 있는 것을 표현한다.

```hs
-- file: ch14/Maybe.hs
instance Monad Maybe where
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    Just _ >> k   =  k
    Nothing >> _  =  Nothing

    return x      =  Just x

    fail _        =  Nothing
```
(>>=) (>>)를 이용해서 여러개의 computation을 chaining할 때 그들 중 하나가 Nothing을 return할 수 있다. 그러면 그 이후는 모두 evaluate되지 않는다.  
그렇긴 하지만, 완전히 chaining이 완전히 줄어든 것은 아니다.  
Nothing이 다음 함수로 넘어가고, 또 그 다음 함수로 끝까지 함수 호출이 이어진다.  
Nothing value를 matching하는 것이 runtime에 싸기는 하지만, 공짜는 아니다.  


##### Executing the Maybe Monad
Maybe monad를 실행하기에 가장 알맞은 함수는 maybe  
첫번 째 오는 b는 3번째 파라미터로 Nothing이 오는 경우 return할 값  
3번째 파라미터가 Just인 경우 첫 파라미터 b는 무시되고 f x 가 수행된 결과를 return 함  
```hs
-- file: ch14/Maybe.hs
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x
``` 
Maybe는 너무 단순해서, maybe 만큼이나 패턴매칭도 많이 사용된다.  
각각 더 readable한 때가 있으므로, 알아서 써라.   


##### Maybe at Work, and Good API Design
고객의 이름으로 빌링 주소 찾기

먼저, 타입 정의
```hs
-- file: ch14/Carrier.hs
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
```

case가 많이 사용되고, 오른쪽으로 쭉쭉 늘어나는 boilerplate가 많은 코드
```hs
-- file: ch14/Carrier.hs
variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap
```

 
```hs
ghci> :module +Data.Map
ghci> :type Data.Map.lookup
Data.Map.lookup :: (Ord k, Monad m) => k -> Map k a -> m a
```
위 정의에서는 map에 데이터가 있었던 경우, Monad 타입에 결과를 넣어주고, 없으면 error를 내는 방식
* 우리가 lookup호출할 때 넘겨준 monad를 기반으로 success, failure를 자동으로 customise할 수 있다.
	* variation1에서는 결과를 Maybe와 비교하고 있다.
* hitch는 잘못된 monad에서 fail을 사용해서 성가신 예외를 던지는 것입니다.  
  이전에 fail 시키는 것에 대해 경고했었으므로, 다시 언급하지 않겠다.

실제로는 모든 lookup의 결과는 Maybe이다.


Maybe가 Monad라는 것을 이용해 더 간단하게 만든 코
```hs
-- file: ch14/Carrier.hs
variation2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address
```

위 코드가 imperitive programmer에게 더 익숙하겠지만, 맨 아래 return address는 redundant 제거하자.  
아래 코드가 더 일반적이다.
```hs
-- file: ch14/Carrier.hs
variation2a person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap
```

partial application을 이용해서 더 간단하게 작성한 모습  
파라미터 순서를 flip을 이용해서 바꿔서 한 줄로 작성할 수 있었다.
```hs
-- file: ch14/Carrier.hs
variation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup = flip M.lookup
```

### The List Monad
Maybe가 1개 혹은 없음을 나타내는 반면, List는 더 다양한 것을 표한하기에 적합하다.  
물론 List를 Monad로 사용할 수 있다.  
Prelude에 List Monad가 정의되어있지만, 어떻게 생겼을지 생각해 보자.  

쉬운 return 부터 생각 해 보자.  m a이므로, [] a 라고 할 수 있겠다. 더 익숙한 표현으로 바꾸면 [a]이다. 
```hs
-- file: ch14/ListMonad.hs
returnSingleton :: a -> [a]
returnSingleton x = [x]
```
(>>=) 는 자연스럽게 아래와 같다. map과 굉장히 비슷하다.
```hs
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

ghci> :type map
map :: (a -> b) -> [a] -> [b]
```
map과 비슷하지만, 파라미터 순서가 맞지 않는데, flip을 이용하면 된다.
```hs
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type flip map
flip map :: [a] -> (a -> b) -> [b]
```
아직 맞지 않는 부분이 있는데, (a -> m b) 와 (a -> b) 이다. concat을 이용해서 맞추자.
```hs
ghci> :type \xs f -> concat (map f xs)
\xs f -> concat (map f xs) :: [a] -> (a -> [a1]) -> [a1]
```

List Monad 정의  
(>>), fail 은 자명하다.
```hs
-- file: ch14/ListMonad.hs
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    xs >> f = concat (map (\_ -> f) xs)
    fail _ = []
```
 
##### Understanding the List Monad
List Monad는 친숙한 Haskell tool인 list comprehension과 유사하다.  
2개의 list를 Cartesian 곱을 계산하는 것을 보자.  
아래 두 코드는 굉장히 유사하고, 결과도 같다.

```hs
-- file: ch14/CartesianProduct.hs
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]
-- file: ch14/CartesianProduct.hs
monadic xs ys = do { x <- xs; y <- ys; return (x,y) }
```
중괄호를 없애고 다시 보자.  
xs의 각 x에 대해 나머지 함수가 모두 실행되고, ys의 각 y에 대해 함수의 나머지 부분이 실행되는 더블중첩 함수이다.  
***중요한 것은, 어떤 모나드 안에서 실행되었는지 모르면, monadic code block이 어떻게 동작할지 예측할 수 없음.***
```hs
-- file: ch14/CartesianProduct.hs
blockyDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)
```

do 도 없애고 좀 더 명확하게 봐보자.
```hs
-- file: ch14/CartesianProduct.hs
blockyPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockyPlain_reloaded xs ys =
    concat (map (\x ->
                 concat (map (\y ->
                              return (x, y))
                         ys))
            xs)
```

##### Putting the List Monad to Work
brute-force 로 두 수의 곱이 어떤 integer n이 되는 모든 positive integer pair를 구하는 함수.
```hs
-- file: ch14/MultiplyTo.hs
guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $
    return (x, y)
```


### Desugaring of do Blocks
하스켈의 do syntax는 syntactic sugar의 예이다. (>>=) 와 anonymous 함수 없이 monadic code를 작성하는 방법을 제공한다.  
Desugaring은 syntactic sugar를 해석해서 core language로 돌려놓는 과정이다.  

do의 desugaring 과정  
컴파일러가 아래 과정을 do keyward가 없어질 때까지 반복하는 것이라고 보면 된다.
  
* do keyword다음에 1개 action이 있는 경우

```hs
-- file: ch14/Do.hs
doNotation1 =
    do act
----------------------------
-- file: ch14/Do.hs
translated1 =
    act
```

* do keyword 다음에 1개 이상의 action 이 있는 경우

```hs
-- file: ch14/Do.hs
doNotation2 =
    do act1
       act2
       {- ... etc. -}
       actN
----------------------------
-- file: ch14/Do.hs
translated2 =
    act1 >>
    do act2
       {- ... etc. -}
       actN

finalTranslation2 =
    act1 >>
    act2 >>
    {- ... etc. -}
    actN
```

* <- notation : 주의해서 볼만한 가치가 있음.  

```hs
-- file: ch14/Do.hs
doNotation3 =
    do pattern <- act1
       act2
       {- ... etc. -}
       actN
-----------------------------------------
-- file: ch14/Do.hs
translated3 =
    let f pattern = do act2
                       {- ... etc. -}
                       actN
        f _     = fail "..."
    in act1 >>= f
```

```hs
-- file: ch14/Do.hs
robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x
-----------------------------------------
robust xs = let f (_:x:_) = do return x
                f _       = fail "..."  // => 여기서 Maybe Monad의 fail 함수 호출. : Nothing
            in Just xs >>= f

ghci> robust [1,2,3]
Just 2
ghci> robust [1]
Nothing
```

* do 안에서 let 키워드 사용 시, in 키워드 생략 가능, 하지만, in 키워드 이후에 왔을 내용들이 let키워드와 줄맞춤 되어야 한다.

```hs
-- file: ch14/Do.hs
doNotation4 =
    do let val1 = expr1
           val2 = expr2
           {- ... etc. -}
           valN = exprN
       act1
       act2
       {- ... etc. -}
       actN
-----------------------------------------
-- file: ch14/Do.hs
translated4 =
    let val1 = expr1
        val2 = expr2
        valN = exprN
    in do act1
          act2
          {- ... etc. -}
          actN
```

##### Monads as a Programmable Semicolon

do, {}, ; 로 이루어진 syntex를 이용하여, align 대신 express를 구분하게 할 수 있다.  
모나드는 일종의 "programmable semicolon" 이라는 apt(automatically programmed tool ??)슬로건을 만들었다. (>>), (>>=) 가 ;과 대치되므로.  
 
```hs
-- file: ch14/Do.hs
semicolon = do
  {
    act1;
    val1 <- act2;
    let { val2 = expr1 };
    actN;
  }
-----------------------------------------
-- file: ch14/Do.hs
semicolonTranslated =
    act1 >>
    let f val1 = let val2 = expr1
                 in actN
        f _ = fail "..."
    in act2 >>= f
```

##### Why Go Sugar-Free?

초보일 때는 더더군다나 syntatic sugar를 사용하지 않는 것이 반복적으로 연습함으로써 어떤 코드를 작성하고 있는지 알기에 좋다.  

(=<<) 는 do를 쓰던 안쓰던 종종 나타난다. (>>=)의 flipped버전이다. 
```hs
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
ghci> :type (=<<)
(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
```

monadic 함수를 right-to-left 스타일로 compose하기에 편리하다.
```hs
-- file: ch14/CartesianProduct.hs
wordCount = print . length . words =<< getContents
```


### The State Monad
앞에서 본 Parse가 monad 였음을 보았는데, 이는 2가지 구분되는 기능을 수행하고 있었다.  
* 파싱에 실패했을 때 구체적인 에러메시지 (Either로 구현했다.)
* implicit state (파싱되고 남은 ByteString)

state를 읽고 쓸수 있는 방법에 대한 요구는 하스켈 프로그램에서 충분히 일반적인 일이고,  
Control.Monad.State라는 모듈에 State monad가 구현되어 있다.  

state를 가지고 우리가 하려는 것은 명확하다.  
어떤 state가 주어지면, 그 state값을 보고 새로운 결과와 새로운 state를 리턴하는 것이다.  
state s, result a 라 하면, s -> (a, s) 일 것이다. 

##### Almost a State Monad
거의 State monad와 비슷한 코드를 만들어보자.  

*  type

```hs
type SimpleState s a = s -> (a, s)
```
state는 상태를 받아서 어떤 결과값과 함께 다른 상태를 내는 함수이다.  
이 때문에 State monad는 state transformer monad라고도 불린다.
monad는 type 파라미터를을 1개만 받는 type constructor를 가져야 하므로,  
아래와 같이 State를 String type으로 partial apply 한다. 
```hs
type StringState a SimpleState String a
```

* return을 정의해보자.

```hs
returnSt :: a -> SimpleState s a
returnSt a = (\s -> (a, s)
```
curring을 이용하여 작성한 함수, 위 함수와 동일하다.
```hs
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)
```
* (>>=)

```hs
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'
```

의미 있는 변수이름을 사용하여, readability를 높인 코드

```hs
-- file: ch14/SimpleState.hs
-- m == step
-- k == makeStep
-- s == oldState

bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    in  (makeStep result) newState
```

타입을 풀어서 실제 타입을 알기 쉽게 보여주는 버전

```hs
bindAlt :: (s -> (a, s))        -- step
        -> (a -> s -> (b, s))   -- makeStep
        -> (s -> (b, s))        -- (makeStep result) newState
```

##### Reading and Modifying the State
State monad의 (>>=), return은 state를 전달하기만 하므로, state를 읽고/변경할 수 있는 함수가 필요하다.

```hs
getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
``` 

##### Will the Real State Monad Please Stand Up?
실제 State monad

* type

constructor에서 s -> (a, s) 를 wrap한 것 밖에 없다.

```hs
newtype State s a = State {
      runState :: s -> (a, s)
    }
```

* return

```hs
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)
```

* (>>=) / bind

```hs
bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'
```
* get/set

```hs
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
```


##### Using the State Monad: Generating Random Values
Parse에서는 state type이 ByteString으로 고정되어있었으나,  
standard library의 State monad는 어떤 타입의 state라도 올 수 있다.  
예를 들어 : State ByteString

pseudorandom value generation 을 구현해보자.

Haskell standard random value generation module : System.Random

IO monad 안에서 돌아가는 버전, C함수의 rand와 유사한 버전.  
randomR은 inclusive range를 받음.
```hs
rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))
```

System.Random에서 제공하는 type classes  
1. RandomGen : 새로운 Random int value에 대한 소스를 정의할 수 있게 해준다.  <br> true random source가 있는 경우, 이 type class 를 이용해서 정의하면 됨.  
2. StenGen : standard RandomGen instance, pseudorandom  
3. Random : 특정 타입의 random value를 어떻게 정의할 지 <br> 일반적인 단순한 타입들에 대해서는 이미 다 정의되어 있다.  

위의 rand코드는 built-in global random generator (IO monad를 상속받고 있다.)를 이용하고 있다. 

##### A First Attempt at Purity
pure하게 random을 사용해 보자.

purity의 어려운점은 random number generator를 get/create 하고 그것이 필요한 곳 까지 전달해야 한다는 것이다.  
거기서 random을 호출하고 나면, 새 random number generator가 return 된다.

동일한 random generator 를 사용하면 동일한 random value가 나온다.
```hs
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
``` 

정상적으로 돌아가는 버전은 좀 그렇다.
```hs
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')
```

이제 state monad를 도입할 때이다.  
IO monad의 global state를 조작하지 않는 다는 것을 보장하는 pure 코드에서 이제 random을 사용해 보자.  
code의 동작을 추론하는데 purity는 매우 중요하다.

##### Random Values in the State Monad

##### Running the State Monad

##### What About a Bit More State?




### Monads and Functors


##### Another Way of Looking at Monads

### The Monad Laws and Good Coding Style




