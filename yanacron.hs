import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Data.List
import Data.Text (replace, pack, unpack)
import Text.Regex.Posix
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
yanacrontab = "./yanacrontab"

spool :: FilePath
spool = "./spool/yanacron/"

-- Data making functions
mkEnv :: [String] -> Env
mkEnv xs = Env {envVar=xs !! 0, value=xs !! 1}

mkJob :: [String] -> Job
mkJob xs = Job {jPeriods=read (xs !! 0), jDelays=read (xs !! 1), jIdent=xs !! 2, jCommand=xs !! 3}

mkPeriodJob :: [String] -> PeriodJob
mkPeriodJob xs = PeriodJob {pjPeriods=xs !! 0, pjDelays=read (xs !! 1), pjIdent=xs !! 2, pjCommand=xs !! 3}

-- Tab line manipulation functions
mergeTabLines :: String -> [String]
mergeTabLines l = lines $ unpack $ replace (pack "\\\n") (pack "") (pack l)

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

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    args <- getArgs
    if length args == 0 then do
        contents <- readFile yanacrontab
        let (envs, jobs, periodJobs) = parseTabLines $ mergeTabLines contents
        mapM_ (forkIO . callCommand . jCommand) jobs
    else return ()

handler :: IOError -> IO ()
handler e = ioError e
