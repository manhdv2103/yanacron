import System.Environment
import System.IO     
import System.IO.Error
import Control.Exception
import Data.Text (replace, pack, unpack)
import Data.List
import Text.Regex.Posix

type Envs = [String]
type Jobs = [String]
type PeriodJobs = [String]
data LineType = EMPTY | ENV | JOB | PERIOD_JOB | OTHER deriving (Eq)

yanacrontab :: FilePath
yanacrontab = "./yanacrontab"

mergeTabLines :: String -> [String]
mergeTabLines l = lines $ unpack $ replace (pack "\\\n") (pack "") (pack l)

classifyLine :: String -> LineType
classifyLine l
    | (l =~ "^[ \t]*($|#)") == True = EMPTY
    | (l =~ "^[ \t]*([^ \t=]+)[ \t]*=(.*)$") == True = ENV
    | (l =~ "^[ \t]*([[:digit:]]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$") == True = JOB
    | (l =~ "^[ \t]*(@[^ \t]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$") == True = PERIOD_JOB
    | otherwise = OTHER

groupTabLines :: [String] -> (Envs, Jobs, PeriodJobs)
groupTabLines = foldl' pushLine ([], [], [])
    where 
    pushLine (e, j, pj) l
        | lineClass == ENV = (e ++ [l], j, pj)
        | lineClass == JOB = (e, j ++ [l], pj)
        | lineClass == PERIOD_JOB = (e, j, pj ++ [l])
        | otherwise = (e, j, pj)
        where lineClass = classifyLine l

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    args <- getArgs
    if length args == 0 then do
        contents <- readFile yanacrontab
        print $ groupTabLines $ mergeTabLines contents
    else return ()

handler :: IOError -> IO ()
handler e = ioError e
