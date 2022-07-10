import System.Environment
import System.IO     
import System.IO.Error
import Control.Exception
import Data.Text (replace, pack, unpack)
import Data.List

yanacrontab :: FilePath
yanacrontab = "./yanacrontab"

mergeTabLines :: String -> [String]
mergeTabLines l = lines $ unpack $ replace (pack "\\\n") (pack "") (pack l)

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    args <- getArgs
    if length args == 0 then do
        contents <- readFile yanacrontab
        mapM_ print $ mergeTabLines contents
    else return ()

handler :: IOError -> IO ()
handler e = ioError e
