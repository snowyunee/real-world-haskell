# Chapter 15 Programming with Monads

## Golfing Practice: Association Lists
* web server와 client 사이에 주고받는 key-value 쌍의 text로 된 목록 정보
	* encoding : application/x-www-form-urlencoded
	* key-value 쌍들은 &로, key, value 는 = 으로 구분
	* 애매한 부분 : value가 반드시 존재해야 하는지 여부
		* value를 Maybe로 표현하면 되겠음.
		* Maybe를 사용함으로써, "no value" 와 "empty value" 를 구분할 수 있
	* 예)
```
 name=Attila+%42The+Hun%42&occupation=Khan
```
* association list
	* alist라고도 함.
	* 위에 예를 alist로 표현하면 아래와 같이
```hs
    [("name",       Just "Attila \"The Hun\""),
     ("occupation", Just "Khan")]
```

* 영화 리뷰 데이터 표현
```hs
data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    }
```
* application/x-www-form-urlencoded 를 영화 리뷰 데이터의 alist 로 만드는 예
	* 필요한 모든 value가 존재하고, 그 값이 모두 empty가 아닐 때 Just MovieReview를 리턴한다.
```hs
-- file: ch15/MovieReview.hs
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
                Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title
```

* 계단현상 제거한 버전

```hs
maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing

```
* lift를 이용
	* pure, monad 함수 섞어쓰기
```hs
liftedReview alist = 
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)
```

## Generalised lifting
liftM-류 함수는 liftM5까지밖에 정의되어있지 않다.  
더 많은 파라미터를 처리하는 liftM10 같은 것을 정의한다 쳐도 언제 그보다 더 많은 파라미터가 필요한 함수가 생길지 모른다.    
* ap
	* monad 안에 파라미터 1개짜리 함수를 어떻게 넣는가? 그리고, 왜 넣는가?
	* 모든 하스켈 함수는 사실 파라미터 1개짜리 함수다.

```hs
ghci> :m +Control.Monad
ghci> :type ap
ap :: (Monad m) => m (a -> b) -> m a -> m b
```
* 이제 MovieReview constructor의 lifting 과정을 보자.
	* MovieReview 생성자
	```hs
	MovieReview :: String -> String -> String -> MovieReview
	```
	* 동일 표현
	```hs
	MovieReview :: String -> (String -> (String -> MovieReview))
	```
	* Maybe 로 lifting
	```hs
	Maybe (String -> (String -> (String -> MovieReview)))
	```
 
* liftM, ap (이거 어려움.)
```hs
apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist
```

## Looking for Alternatives (대체법 찾기)
* 전화번호 데이터 정의 및 예

```hs
data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]
```
* 사적인 전화를 위해 business는 제외하고, home -> mobile 순으로 전화번호를 찾는 함수

```hs
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n
```
* business -> mobile순으로 찾는 함수, 여러 전화번호가 있을 수 있으니, Maybe가 아닌 []로 변경

```hs
allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

contextIs a (b, _) = a == b
```
* MonadPlus

```hs
class Monad m => MonadPlus m where
   mzero :: m a	
   mplus :: m a -> m a -> m a
```
* Maybe의 MonadPlus 구현 예

```hs
instance MonadPlus [] where
   mzero = []
   mplus = (++)

instance MonadPlus Maybe where
   mzero = Nothing

   Nothing `mplus` ys  = ys
   xs      `mplus` _ = xs
```
* MonadPlus 를 이용해 case 를 제거하고 단순화 시킨 버전

```hs
oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
```
* Maybe 타입을 반환하는 lookup의 구현은 당연하다.

```hs
-- file: ch15/VCard.hs
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ []                      = Nothing
lookup k ((x,y):xys) | x == k    = Just y
                     | otherwise = lookup k xys
```
* MonadPlus instance인 Monad 타입으로 일반화 시키기
	* 이제 Maybe 타입이었으면 한개만 리턴하고, [] 타입이었으면 해당하는 모든 것을 반환한느 코드가 단순하게 될 수 있겠다.
		* 적어보면 좋겠네~ 

```hs
-- file: ch15/VCard.hs
lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ []    = mzero
lookupM k ((x,y):xys)
    | x == k    = return y `mplus` lookupM k xys
    | otherwise = lookupM k xys
```


### The name mplus does not imply addition (mplus는 더하기를 의미하는 것이 아니다.)
어떤 Monad이냐에 따라 mplus의 구현이 다르다.  
mplus에 "plus"라는 문자열이 들어있다고 두 값을 더 한다고 생각해선 안된다.
```hs
ghci> [1,2,3] `mplus` [4,5,6]
[1,2,3,4,5,6]

ghci> Just 1 `mplus` Just 2
Just 1
``` 
### Rules for working with MonadPlus (MonadPlus의 법칙)
1. rule 1 : mzero가 >>=(bind)왼쪽에
```hs
mzero >>= f == mzero
```
2. rule 2 : mzero가 >> 오른쪽에
```hs
  v >> mzero == mzero
```

### Failing safely with MonadPlus (MonadPlus로 안전하게 실패하기)
이전에 14장 The Monad typeclass에서 fail 을 소개했었는데, 대부분 error로 구현해서 사용하지 말 것을 권장했었다.  
MonadPlus를 이용해서 fail, error 없이 안전하게 실패시켜보자.  
 
```hs
-- Control.Monad 에 정의되어 있다.
guard        :: (MonadPlus m) => Bool -> m ()
guard True   =  return ()
guard False  =  mzero
```

* 예) x % z 가 0이면, return x, 아니면 mzero 

```hs
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
```

## Adventures in Hiding Plumbing (배관 감추기 모험)
* "Using the State Monad: Generating Random Value" 에서 보았던 State monad 사용법의 단점  
	* State monad 저자와 사용자가 동일한 수준으로 상태값에 접근 가능해서,  
	  버그 들어갈 수 있다.
	* 내부 코드에 의존하는 사용이 생길 수 수 있다.  
	  이후 구현을 수정하고 싶어도 못 고치게 된다.
* Random number monad의 구현 상세 감추기
	* State monad 감추기
		* State monad의 state를 사용자가 마음데로 수정할 수 없도록
* 스코프 확대(기능 확대?)
	* supply unique values of any kind (어떤 종류의 데이터이던지 unique한 값을 제공하겠다.)
		* 이전에는 랜덤 숫자만 제공했었다.
	* value(정의에서 a에 해당)의 타입이 어떤 것이든 가능하다
		* random number, names for temporary files, or identifiers for HTTP cookies ..
	* Supply monad 정의 (type constructor)
	* runSupply (execution function)
		* excution 함수
		* 값들의 목록을 가지고 실행, 이 목록의 값들이 unique함이 보장되어야 한다.
	* next (action)
		* Supply 모나드 안에서 실행 가능 
		* next를 실행할 때마다 값들의 목록에서 다음 값(s)을 건내준다.
		* 다음 값이 없을 수 있으므로 Maybe 타입으로 한다.

```hs
runSupply :: Supply s a -> [s] -> (a, [s])

next :: Supply s (Maybe s)

newtype Supply s a = S (State [s] a)

module Supply
    (
      Supply
    , next
    , runSupply
    ) where

```

* Supply monad 정의
	* interface가 단순해짐
	* State monad를 내부에 감추는 정의도 매우 단순함
	* 타입 파라미터
		* s
			* 우리가 제공해야 하는 unique한 값들의 타입
		* a
			* Supply가 monad 이기 위해 제공되어야 하는 일반적인 타입 파라미터
	* Monad typeclass의 instance
		* 이를 위한 함수 정의는 State Monad의 함수를 그대로 이용
		* (>>=), return
		* GeneralizedNewtypeDeriving
			* monad를 instatnce하기 위한 boiler plate는 필요없다.
			* language extention of GHC
			* newtype으로 선언한 데이터 타입이 typeclass instance를 derive하게 해준다.
	* 이제 next, runSupply만 구현하면 된다. (이건 사제 함수니까)

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
newtype Supply s a = S (State [s] a)
    deriving (Monad)

--###################################################
-- Supply가 Monad의 instance가 되기 위한 기계적인 코드
--###################################################
unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Monad (Supply s) where
    (>>=) :: m a -> (a -> m b) -> m b
    -- 왜 이게 아닌 것인지?
    --   s >>= m = unwrapS s >>= m
    --   이유 >>= 가 state의 >>= 이므로, >>=의 타입을 맞춰야 하니까 다시 까서 싸줘야 하는거다. 
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return
--###################################################

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)
-- runState :: s -> (a, s)
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs
``` 

* Supply monad 실행 예

```hs
ghci> :load Supply
[1 of 1] Compiling Supply           ( Supply.hs, interpreted )
Ok, modules loaded: Supply.
ghci> runSupply next [1,2,3]
Loading package mtl-1.1.0.0 ... linking ... done.
(Just 1,[2,3])
ghci> runSupply (liftM2 (,) next next) [1,2,3]
((Just 1,Just 2),[3])
ghci> runSupply (liftM2 (,) next next) [1]
((Just 1,Nothing),[])
```

* Supply 가 내부 State에 대해 외부에 노출시키지 않는 다는 것을 확인

```hs
ghci> :browse Supply
data Supply s a
next :: Supply s (Maybe s)
runSupply :: Supply s a -> [s] -> (a, [s])
ghci> :info Supply
data Supply s a 	-- Defined at Supply.hs:17:8-13
instance Monad (Supply s) -- Defined at Supply.hs:17:8-13
```

### Supplying Random Numbers

* 랜덤 넘버 제너레이터 작성
	* 어려운 점
		* 무한 list를 다뤄야 하겠다. 계속 랜덤값을 줬으면 하니까.
	* IO monad에서 StdGen을 이용하여 random 값을 "get" 나서 다른 StdGen을 "set" 해야 한다.  
      그렇지 않으면 우리가 얻은 것과 같은 random값을 이후에 얻게된다.

```hs
getStdRandom :: (StdGen -> (a, StdGen)) -> IO a

import Supply
import System.Random hiding (next)

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
```

* 실행 예
	* randomsIO 가 2번째 인자로 무한 list를 리턴하므로, fst를 해야 무한 list를 print 하지 않는다.
	* 이제 매번 실행할 때마다 새로운 랜덤 값을 준다. (하지만 현실은 컴파일 에러)

```hs
ghci> :load RandomSupply
[1 of 2] Compiling Supply           ( Supply.hs, interpreted )
[2 of 2] Compiling RandomSupply     ( RandomSupply.hs, interpreted )
Ok, modules loaded: RandomSupply, Supply.
ghci> (fst . runSupply next) `fmap` randomsIO

<interactive>:1:17:
    Ambiguous occurrence `next'
    It could refer to either `Supply.next', imported from Supply at RandomSupply.hs:4:0-12
                                              (defined at Supply.hs:32:0)
                          or `System.Random.next', imported from System.Random
ghci> (fst . runSupply next) `fmap` randomsIO

<interactive>:1:17:
    Ambiguous occurrence `next'
    It could refer to either `Supply.next', imported from Supply at RandomSupply.hs:4:0-12
                                              (defined at Supply.hs:32:0)
                          or `System.Random.next', imported from System.Random
```

### Another Round of Golf (또 다른 예제)
* 하스켈에서 pair중에 앞에 것에만 연산을 하거나, 뒤에 것에만 연산을 하는 일은 흔하다.

```hs
ghci> :m +Control.Arrow
ghci> first (+3) (1,2)
(4,2)
ghci> second odd ('a',1)
('a',True)
```

* 우리의 랜덤 'a' 타입 제너레이터를 1줄로 간단한 코드로 만들어보자.

```hs
import Control.Arrow (first)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)
``` 

## Separating Interface from Implementation (인터페이스와 구현 분리하기)

* System.Random 의 randomsIO는 성능이 좋지 않다. 이를 이용하여 next를 구현하면 당연히 성능이 좋지않다.
* 랜덤 넘버 소스를 변경할 수 있게 하자. 

```hs
class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
```

* 아직 설명 안된 부분이 있어 궁금할텐데 아래 알려주고 있으니 읽어라.


### Multiparameter Typeclasses

```hs
class (Monad m) => (MonadSupply s) m | m -> s where
    next :: m (Maybe s)
```

* "MonadSupply s m" typeclass를 어떻게 읽어야 할까?
	* (MonadSupply s) m 이라고 보면 좀 더 명확하다.
	* m은 monad인데, m는 다른 일반적인 typeclass 들과 달리 parameter를 하나 가지고 있다.
* MultiParamTypeClasses
	* 여기 잘 모르겠음.
		* 아마 MonadSupply s m 이는 즉, MonadSupply s m a 이므로, m a 2개나 정해지지 않은 타입이 있다는 것을 의미하는 건가?
		* 아닌 것 같다. s m 2개를 모른다는 뜻인 것 같다.
	* s
		* MonadSupply 에서 s는 Supply에서의 s와 동일한 의미로 사용되었고,  
		  이는 next 에서 건내주는 value의 타입니다.
	* typeclass's context (super class)
		* m이 이미 Monad이므로, MonadSupply s 는 >>=, return을 구현할 필요 없다.
		* 그럼 (>>=), return 이 그 m의 (>>=), return 을 자동으로 사용하게 된다는 뜻인가?


### Functional Dependencies

* "| m -> s"
	* 이 것이 functional dependency
	* | : "such taht"이라고 읽을 수 있다.
	* -> : "uniquely dtermines" 라고 읽을 수 있다.
* haskell type checker가 근본적으로 "theorem prover"임을 상기해보자.
	* nonterminating proof 의 컴파일 결과는 컴파일러가 컴파일을 포기하거나 무한루프에 빠지는 것일 거다.
* functional dependency를 적어주지 않았다면 위 코드는 컴파일 오류가 났을 것이다.
* 다음을 보자.
	* 질문? : S.Supply s a 인데 a는 그럼 생략된 것인가?
	* m -> s 를 안적었다면, s와 S.Supply s 와의 관계를 알 수 없었을 것이다.
		* S.Supply s 
		* 컴파일 에러도 나고, 처음 그것을 쓰려는 시도가 있기 전까지 컴파일 에러도 안난다.

```hs
import qualified Supply as S

-- Q? : instance MonadSupply s (S.Supply s a) a where 
--      라고 해야 하는 것은 아닌가?
--    => 원래 a는 안 적는건가보다. Maybe 의 정의도 아래와 같다.
--       instance  Monad Maybe  where 
instance MonadSupply s (S.Supply s) where

    next = S.next

```

* S.Supply Int라는 것을 라는 타입을 생각해 보자
	* functional dependency가 없었다면, MonadSupply s 의 s가 Int 라는 것을 컴파일러는 몰랐을 것이다.
	* 컴파일은 성공하고, MonadSupply를 처음 사용할 때 에러가 발생했을 것이다.


* Functional dependency가 이해하기 어려운데 이번에 소개한 것과 같은 간단한 사용이 대부분이므로 걱정하지 말아라.

### Rounding Out Our Module
* SupplyClasss.hs는 아래와 같이 생겼다.

```hs
{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses #-}

module SupplyClass
    (
      MonadSupply(..)
    , S.Supply
    , S.runSupply
    ) where
```


* FlexibleInstances extension
	* 필수적이다.
		* 몇몇 환경에서 instances를 작성하는 데 일반적인 룰들을 느슨하게 해준다.
		* 그러면서도 컴파일러가 타입체크를 완료할 수 있게 해준다.
	* 여기서 필요했던 이유
		* functional dependancy를 사용하기 위해
		* 상세한 설명은 이 책의 스코프를 넘어선다.
* language extension이 필요한 때를 어떻게 알 수 있는가?
	* GHC 가 추천해준다.
	* -XFlexibleInstances 같은걸 넣어서 컴파일해보라고 한다.  
	  이는 {-# LANGUAGE FlexibleInstances #-} 를 넣는 것과 동일한 효과이다.

* re-exporting
	* runSupply, Supplyfmf re-exporting했다.
	* 이렇게 함으로써 사용자는 SupplyClass 모듈만 import하면된다.
	* "moving parts" 를 줄여준다.
		* 움직이는 부분? 주의해야 하는 부분?  

### Programming to a Monad's Interface
* Supply monad를 이용하여 2개의 값을 가져와서 스트링 포맷으로 리턴하는 예

```hs
showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
``` 

* 위 예는 Supply monad에 사용이 국한된다.  
  MonadSupply interface를 구현한 어떤 monad 라도 사용가능하게 고쳐보자.  
  그럼에도 함수의 body 부분은 변경되지 않았다.

```hs
showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
```

## The Reader Monad

## A Return to Automated Deriving



## Hiding the IO Monad

### Designing for Unexpected Uses

### Using Typeclasses

### Isolation and Testing

### The Writer Monad and Lists

### Arbitrary I/O Revisited
