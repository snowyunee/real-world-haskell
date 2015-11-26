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