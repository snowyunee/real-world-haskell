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
