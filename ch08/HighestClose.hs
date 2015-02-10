-- file: ch08/HighestClose.hs
--
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment
import System.Exit

closing = readPrice . (!!4) . L.split ','

-- file: ch08/HighestClose.hs
readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case L.readInt (L.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)

-- file: ch08/HighestClose.hs
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)


highestCloseFrom2 contents = do
    print (highestClose contents)


parseArgs ["-h"]    = usage >> exit
parseArgs ["-v"]    = version >> exit
parseArgs []        = L.getContents
parseArgs (fs:args) = L.readFile fs


usage     = putStrLn "Usage: HighestClose [-vh] [file]";
version   = putStrLn "version: 1.0";
exit      = exitWith ExitSuccess
die       = exitWith (ExitFailure 1)


main = do
    getArgs >>= parseArgs >>= print.highestClose

