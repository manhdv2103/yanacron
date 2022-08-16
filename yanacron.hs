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
import System.FileLock
import System.FilePath.Posix (takeDirectory)
import System.IO     
import System.IO.Error
import System.Process
import qualified Data.ByteString.Char8 as B -- Use this to read/write file. Reason: https://stackoverflow.com/questions/13097520/error-reading-and-writing-same-file-simultaneously-in-haskell
import Data.ByteString.UTF8 (toString, fromString)

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

type InvalidParsedLine = Int
type ParseTabGroup = ([Env], [Job], [PeriodJob], [InvalidParsedLine])

-- File and directory

yanacrontab :: FilePath
yanacrontab = "/home/vumanh/Documents/yanacron/yanacrontab"

spool :: FilePath
spool = "/home/vumanh/Documents/yanacron/spool/yanacron/"


-- Data making functions

mkEnv :: [String] -> Env
mkEnv xs = Env (xs !! 0) (xs !! 1)

mkJob :: [String] -> Job
mkJob xs = Job (read (xs !! 0)) (read (xs !! 1)) (xs !! 2) (xs !! 3)

mkPeriodJob :: [String] -> PeriodJob
mkPeriodJob xs = PeriodJob (xs !! 0) (read (xs !! 1)) (xs !! 2) (xs !! 3)


-- Merge lines separated by the backslash (\)
mergeTabLines :: String -> [String]
mergeTabLines l = lines $ unpack $ replace (pack "\\\n") (pack "") (pack l)

-- Parse tab lines into environment variables or jobs
-- Regexes taken from https://github.com/cronie-crond/cronie/blob/master/anacron/readtab.c#L287
parseTabLines :: [String] -> ParseTabGroup
parseTabLines = foldl' pushLine ([], [], [], []) . createLineNumber
    where 
    createLineNumber = zip [1..]
    pushLine :: ParseTabGroup -> (Int, String) -> ParseTabGroup
    pushLine (e, j, pj, el) (ln, l)
        | l =~ "^[ \t]*($|#)" = (e, j, pj, el) -- Ignore empty line
        | let res = (l =~ "^[ \t]*([^ \t=]+)[ \t]*=(.*)$"), not $ null res = (e ++ [mkEnv $ extractMatches res], j, pj, el) -- Read environment line
        | let res = (l =~ "^[ \t]*([[:digit:]]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$"), not $ null res = (e, j ++ [mkJob $ extractMatches res], pj, el) -- Read job line
        | let res = (l =~ "^[ \t]*(@[^ \t]+)[ \t]+([[:digit:]]+)[ \t]+([^ \t/]+)[ \t]+([^ \t].*)$"), not $ null res = (e, j, pj ++ [mkPeriodJob $ extractMatches res], el) -- Read period job line
        | otherwise = (e, j, pj, el ++ [ln])
    extractMatches = tail . head

-- Parse the date with the correct format
parseDate :: String -> Maybe Day
parseDate s = parseTimeM True defaultTimeLocale "%0Y%m%d" s :: Maybe Day

-- Print warning of invalid lines from the yanacrontab file
printInvalidLinesWarning :: [InvalidParsedLine] -> IO ()
printInvalidLinesWarning = mapM_ (\n -> putStrLn $ "yanacron: Invalid syntax in " ++ yanacrontab ++ " on line " ++ show n ++ " - skipping this line")

-- Decide if the job will be run today or not
willRun :: Int -> Day -> Maybe Day -> Bool
willRun period currentDay lastRun = case lastRun of
    Nothing -> True
    Just val -> daysDiff < 0 || daysDiff >= fromIntegral period
        where daysDiff = diffDays currentDay val

-- Create file if file does not exist
-- Function taken and modified from https://stackoverflow.com/questions/58682357/how-to-create-a-file-and-its-parent-directories-in-haskell
createFile :: FilePath -> IO ()
createFile path = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path ""

-- Create the command to run in the shell
createCommand :: Job -> Day -> String
createCommand job currentDay = "/bin/sh -c \"" ++
    (unpack $ replace (pack "\"") (pack "\\\"") (pack $ jCommand job)) ++ "\""

-- Run the job, return True if the job was run, False otherwise
runJob :: UTCTime -> Job -> IO Bool
runJob currentTime job = do
    let spoolFile = spool ++ (jIdent job)
    --fileExist <- doesFileExist spoolFile

    result <- withTryFileLock spoolFile Exclusive (\x -> do
        lastRunStr <- fmap toString $ B.readFile spoolFile

        let currentDay = utctDay currentTime :: Day
            lastRun = parseDate $ take 8 lastRunStr
            run = willRun (jPeriods job) currentDay lastRun

        when run $ do
            createFile $ spool ++ jIdent job
            threadDelay $ 60000000 * jDelays job -- 60000000 (microseconds) is equal to 1 minute

            callCommand $ createCommand job currentDay
            B.writeFile spoolFile . fromString $ formatTime defaultTimeLocale "%0Y%m%d" currentDay ++ "\n"
            putStrLn ("End: " ++ show run ++ " " ++ createCommand job currentDay)

        return run)

    if result == Nothing then putStrLn ("Job '" ++ jIdent job ++ "' locked by another yanacron - skipping")
    else return ()

    return $ maybe False id result

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    args <- getArgs
    if length args == 0 then do
        currentTime <- getCurrentTime

        putStrLn $ "Yanacron started on " ++ formatTime defaultTimeLocale "%0Y-%m-%d" currentTime

        contents <- readFile yanacrontab
        let (envs, jobs, periodJobs, errorLines) = parseTabLines $ mergeTabLines contents

        printInvalidLinesWarning errorLines

        jobsRun <- mapConcurrently (runJob currentTime) jobs 

        -- Taken from https://stackoverflow.com/questions/573751/using-foldl-to-count-number-of-true-values
        let totalJobsRun = sum $ map fromEnum jobsRun
        
        putStrLn ("Normal exit (" ++ show totalJobsRun ++ " job" ++ (if totalJobsRun == 1 then "" else "s") ++ " run)")
        return ()
    else return ()

handler :: IOError -> IO ()
handler e = do 
    ioError e
