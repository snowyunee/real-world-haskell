# Chapter 16 Using parsec

프로그래머에게 parsing이라는 것은 일상적으로 하는 일이다. 이 것에 관해 regular expression을 배웠는데, 이 것도 충분히 좋지만 만능이 아니고 조금만 더 볶잡해 져도 이만으로는 충분치 않다. 다른 것을 배워보자.

Parsec은 파서들을 엮을 수 있는 라이브러리이다. 이를 이용하여 더 수준높은 파서를 작성할 수 있다.

Parsec은 어휘 분석과 파싱 둘 다 수행한다.

 

## First Steps with Parsec: Simple CSV Parsing


```hs
import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
```
* cells가 line이나 csvFile처럼 구현되지 못한 이유
    * ',' 가 있을 수도 있고 없을 수도 있어서
* <|> Parsec에서 왼쪽 함수가 소진한게 없으면 오른쪽 함수를 수행함.

* 실행 예

```hs
ghci> parseCSV "hi\n"
Right [["hi"]]
ghci> parseCSV "line1\nline2\nline3\n"
Right [["line1"],["line2"],["line3"]]
ghci> parseCSV "cell1,cell2,cell3\n"
Right [["cell1","cell2","cell3"]]
ghci> parseCSV "l1c1,l1c2\nl2c1,l2c2\n"
Right [["l1c1","l1c2"],["l2c1","l2c2"]]
ghci> parseCSV "Hi,\n\n,Hello\n"
Right [["Hi",""],[""],["","Hello"]]
```

## The sepBy and endBy Combinators

* sepBy
	* 함수 2개 받고, 앞에것이 컨텐츠, 뒤에 것이 구분자를 파싱한다. 뒤에 함수가 더 이상 파싱할 수 없을 때 까지 계속한다.
		* 따라서 구분자가 마지막에 올 수도 있고 안올 수도 있다.  
		* 질문 : (char ',') 이것이 함수인가? => 모나드이다??  

```hs
*Main> :type char
char
  :: Text.Parsec.Prim.Stream s m Char =>
     Char -> Text.Parsec.Prim.ParsecT s u m Char

*Main> :type (char 'a')
(char 'a')
  :: Text.Parsec.Prim.Stream s m Char =>
     Text.Parsec.Prim.ParsecT s u m Char
```

* endBy
	* 함수 2개 받고, 앞에 것이 컨텐츠를 파싱한다. 컨텐츠를 더 이상 파싱할 수 없을 때까지 계속하고, 뒤에 함수가 반드시 파싱에 성공해야 한다.
		* 따라서, 마지막 구분자가 반드시 있어야 한다.

## Choices and Errors
* OS에 따라 서로 다른 eol 처리하기
	* Unix/Linux system, Windows 텍스트 모드 : "\r"
	* DOS, Windows system : "\n"
	* Macs : "\r"
	* 우리가 추가로 서포트 할 것 : "\n\r"

* 1번 : "\n\r" 을 "\n" 으로 파싱하고, 다음 문장이 \r부터 시작하는 문제가 있다.
```hs
ghci> let eol = string "\n" <|> string "\n\r"
ghci> parse eol "" "\n"
Right "\n"
ghci> parse eol "" "\n\r"
Right "\n"
```
* 2번 : 1번 문제 해결을 위한 방안. "\n" 만 오는 경우, 왼쪽 함수에서 "\n"을 소비해 버려서 문제 발 
```hs
ghci> let eol = string "\n\r" <|> string "\n"
``` 
* 3번 : '\n'을 찾고 이게 있으면 '\r'을 찾는다, '\r'이 없으면 이미 '\n'은 소비했으므로, 바로 return '\n'으로 끝낸다.
```hs
-- file: ch16/csv5.hs
eol = 
    do char '\n'
       char '\r' <|> return '\n'
```

### Lookahead
* try
	* 함수를 하나 받는데, 실패하면 스트림을 소진하지 않은 것 처럼 동작

이제 <|> 를 이용하여 쉽게 작성하는 것이 가능

```hs
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
```

### Error Handling

* <?> 에러 메시지 추가

```hs
-- file: ch16/csv8.hs
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- 에러메시지 변화

ghci> parseCSV "line1"
Left "(unknown)" (line 1, column 6):
unexpected end of input
expecting ",", "\n\r", "\r\n", "\n" or "\r"


ghci> parseCSV "line1"
Left "(unknown)" (line 1, column 6):
unexpected end of input
expecting "," or end of line

``` 


## Extended Example: Full CSV Parser (CSV 파서 풀버전)

CSV파일은 콤마(,)로 셀을 구분하는데 셀에 콤마(,)가 들어가는 경우 어떻게 처리할까.  
셀의 처음과 끝에 " 를 붙인다.  
셀 내용에 " 가 들어갈 때는? => "" 로 만든다.  
이 부분이 보완되어 처리된 코드가 아래 내용이다.

```hs
-- file: ch16/csv9.hs
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
```

실제 파싱 코드는 21 라인밖에 안된다. 

이 코드에서 try가 유용하게 쓰였다. quotedChar 함수에서 보면 된다.


## Parsec and MonadPlus

