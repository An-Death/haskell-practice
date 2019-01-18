import Control.Monad (liftM)
import Data.Foldable (foldrM)
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath.Windows (dropTrailingPathSeparator, pathSeparator, takeExtension)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Data.ByteString.Char8         as C (pack)   
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Codec.Compression.GZip        as GZip

basePath dir = (dropTrailingPathSeparator dir)
withBase dir = (++) ((basePath dir) ++ [pathSeparator])
newLine = BI.c2w '\n'

byteSplitLines :: BL.ByteString -> [BL.ByteString]
byteSplitLines = BL.split newLine

readFile' :: FilePath -> IO [BL.ByteString]
readFile' file
        | takeExtension file == ".gz" = readGziped file
        | otherwise                   = readSimple file
    where 
        readSimple = fmap byteSplitLines . BL.readFile
        readGziped =  fmap (byteSplitLines . GZip.decompress) . BL.readFile


readF :: FilePath -> IO ()
readF file = do 
    content <- readFile' file
    mapM_ print content

writeToFile :: FilePath -> BL.ByteString -> IO()
writeToFile filename = BL.writeFile filename

toStrict :: BL.ByteString -> B.ByteString
toStrict BLI.Empty = B.empty
toStrict (BLI.Chunk c BLI.Empty) = c
toStrict lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)


filterPatterns = map C.pack [" reject:", "client=", "warning: header Subject", "TLS connection established from"]
filterRecords :: B.ByteString -> Bool
filterRecords x = any ($ x) (map B.isInfixOf filterPatterns)

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' f acc []      = acc
foldr' f acc (x:xs)  = x `f` foldr f acc xs

processFileData :: [BL.ByteString] -> BL.ByteString
processFileData = foldr f BLI.Empty
        where 
            _newLine = BL.pack [newLine]
            f str acc 
                | (filterRecords . toStrict) str = BL.append acc (BL.append str _newLine)
                | otherwise = acc

processFiles :: FilePath -> FilePath -> IO ()
processFiles dir toFile = do
    files <- listDirectory dir
    printf "TotalFiles: %i \n" ((length files)::Int)
    fileData <- mapM readFile' . reverse $ map (withBase dir) files
    let content = foldr (\filedata acc -> BL.append acc (processFileData filedata) ) BLI.Empty fileData
    writeToFile toFile . GZip.compress $ content

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