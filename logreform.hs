
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.List (isInfixOf, intercalate)
import System.Directory
import System.FilePath.Windows (dropTrailingPathSeparator, pathSeparator, takeExtension)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf
import qualified Data.ByteString.Lazy as ByteString
import qualified Codec.Compression.GZip as GZip

filterPatterns = [" reject:", "client=", "warning: header Subject", "TLS connection established from"]

dropDots =  filter (\f -> f /= "." && f /="..")
flatten = intercalate []
basePath dir = (dropTrailingPathSeparator dir)
withBase dir = (++) ((basePath dir) ++ [pathSeparator])

readGziped :: FilePath -> IO [String]
readGziped =  (fmap (lines . unpack . GZip.decompress)) . ByteString.readFile

readFile' :: FilePath -> IO [String]
readFile' file
    | takeExtension file == ".gz" = readGziped file
    | otherwise = (fmap (lines . unpack)) . ByteString.readFile $ file

readF :: FilePath -> IO ()
readF file = do 
    content <- readFile' file
    mapM_ print content

writeToFile :: FilePath -> ByteString.ByteString -> IO()
writeToFile filename = ByteString.writeFile filename

filterRecords :: [Char] -> Bool
filterRecords = (\x -> any ($ x) (map isInfixOf filterPatterns))

processFiles :: FilePath -> FilePath -> IO ()
processFiles dir toFile = do
    files <- getDirectoryContents dir
    let file = map (withBase dir) $ (reverse . dropDots) files
    printf "TotalFiles: %i \n" ((length file)::Int)
    listOfContents <- mapM readFile' file
    let filtered = filter filterRecords $ flatten listOfContents
    writeToFile toFile . GZip.compress . pack . unlines $ filtered

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v 

main :: IO()
main = do
    putStrLn "Starting..."
    [dir, toFile] <- getArgs
    printf "Path: %s -> %s \n" dir toFile
    time $ processFiles dir toFile
    putStrLn "Done."