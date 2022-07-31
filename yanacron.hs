import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (replace, pack, unpack)
import Data.Time
import Text.Regex.Posix
import System.Directory
import System.Environment
import System.IO     
import System.IO.Error
import System.Process

-- Data & Type

data Env = Env { envVar :: String
                , value :: String
                } deriving (Eq, Show, Read)
data Job = Job { jPeriods :: Int
                , jDelays :: Int
                , jIdent :: String
                , jCommand :: String
                } deriving (Eq, Show, Read)
data PeriodJob = PeriodJob { pjPeriods :: String
                , pjDelays :: Int
                , pjIdent :: String
                , pjCommand :: String
                } deriving (Eq, Show, Read)

type Envs = [Env]
type Jobs = [Job]
type PeriodJobs = [PeriodJob]


-- File and directory

yanacrontab :: FilePath
yanacrontab = "/home/vumanh/Documents/yanacron/yanacrontab"

spool :: FilePath
spool = "/home/vumanh/Documents/yanacron/spool/yanacron/"


-- Data making functions

mkEnv :: [String] -> Env
mkEnv xs = Env {envVar=xs !! 0, value=xs !! 1}

mkJob :: [String] -> Job
mkJob xs = Job {jPeriods=read (xs !! 0), jDelays=read (xs !! 1), jIdent=xs !! 2, jCommand=xs !! 3}

mkPeriodJob :: [String] -> PeriodJob
mkPeriodJob xs = PeriodJob {pjPeriods=xs !! 0, pjDelays=read (xs !! 1), pjIdent=xs !! 2, pjCommand=xs !! 3}


-- Merge lines separated by the backslash (\)
mergeTabLines :: String -> [String]
mergeTabLines l = lines $ unpack $ replace (pack "\\\n") (pack "") (pack l)

-- Parse tab lines into environment variables or jobs
parseTabLines :: [String] -> (Envs, Jobs, PeriodJobs)
parseTabLines = foldl' pushLine ([], [], [])
    where 
    pushLine :: (Envs, Jobs, PeriodJobs) -> String -> (Envs, Jobs, PeriodJobs)
    pushLine (e, j, pj) l
        | let res = (l =~ "^[ \t]*([^ \t=]+)[ \t]*=(.*)$"), not $ null res == True = (e ++ [mkEnv $ extractMatches res], j, pj) -- Read environment line
        | let res = (l =~ "^[ \t]*([[:digit:]]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$"), not $ null res = (e, j ++ [mkJob $ extractMatches res], pj) -- Read job line
        | let res = (l =~ "^[ \t]*(@[^ \t]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$"), not $ null res = (e, j, pj ++ [mkPeriodJob $ extractMatches res]) -- Read period job line
        | otherwise = (e, j, pj)
    extractMatches = tail . head

-- Parse the date with the correct format
parseDate :: String -> Maybe Day
parseDate s = parseTimeM True defaultTimeLocale "%0Y%m%d" s :: Maybe Day

-- Decide if the job will be run today or not
willRun :: Int -> Day -> Maybe Day -> Bool
willRun period currentDay lastRun = case lastRun of
    Nothing -> True
    Just val -> daysDiff < 0 || daysDiff >= fromIntegral period
        where daysDiff = diffDays currentDay val

createCommand :: Job -> Day -> String
createCommand job currentDay = "/bin/sh -c \"" ++
    (unpack $ replace (pack "\"") (pack "\\\"") (pack $ jCommand job)) ++ "\"; echo '" ++
    (formatTime defaultTimeLocale "%0Y%m%d" currentDay) ++ "' > " ++ spool ++ jIdent job

--runCommand :: String -> IO ()

-- Run the job
runJob :: Job -> IO()
runJob job = do
    currentTime <- getCurrentTime

    let spoolFile = spool ++ (jIdent job)
    fileExist <- doesFileExist spoolFile

    lastRunStr <- if fileExist
        then readFile spoolFile
        else return ""

    let currentDay = utctDay currentTime :: Day
        lastRun = parseDate lastRunStr
        run = willRun (jPeriods job) currentDay lastRun

    when run $ do
        threadDelay $ 60000000 * jDelays job

        callCommand $ createCommand job currentDay
        putStrLn ("End: " ++ show run ++ " " ++ createCommand job currentDay)

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    args <- getArgs
    if length args == 0 then do
        tabExist <- doesFileExist yanacrontab
        if tabExist
            then do
                contents <- readFile yanacrontab
                let (envs, jobs, periodJobs) = parseTabLines $ mergeTabLines contents
                mapConcurrently runJob jobs
                return ()
            else putStrLn "No yanacrontab available!"
    else return ()

handler :: IOError -> IO ()
handler e = ioError e
