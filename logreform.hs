{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath.Windows (dropTrailingPathSeparator, pathSeparator, takeExtension)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf
import System.IO
import qualified Codec.Compression.GZip        as GZip
import qualified Data.ByteString.Lazy          as ByteString 
import qualified Data.ByteString.Lazy.Char8    as ByteString (lines, unlines)
import qualified Data.ByteString               as ByteStringStrict

basePath dir = (dropTrailingPathSeparator dir)
withBase dir = (++) ((basePath dir) ++ [pathSeparator])

isGz file = takeExtension file == ".gz"

patterns :: [ByteStringStrict.ByteString]
patterns = [" reject:", "client=", "warning: header Subject", "TLS connection established from"]
filterLines :: [ByteString.ByteString] ->  [ByteString.ByteString]
filterLines = filter isValid 
    where 
        isValid x = any ($ ByteString.toStrict (x)) filterPatterns
        filterPatterns = map ByteStringStrict.isInfixOf patterns

 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v 

writeCompressed :: FilePath -> ByteString.ByteString -> IO ()
writeCompressed toFile = write . GZip.compress
    where
        write = ByteString.appendFile file
        file = if isGz toFile then toFile else toFile ++ ".gz"

filterFile :: ByteString.ByteString -> ByteString.ByteString
filterFile = ByteString.unlines . filterLines . ByteString.lines


main :: IO()
main = do
    putStrLn "Starting..."
    [dir, toFile] <- getArgs
    printf "Path: %s -> %s.gz \n" dir toFile
    files <- listDirectory dir
    printf "TotalFiles: %i \n" (length files)

    time $ do
        withFile (toFile ++ ".gz") WriteMode (\wH ->
            forM_ (reverse files) (\file -> 
                let 
                    path = (withBase dir file)
                    reader = whichReader file
                in withFile path ReadMode (\h -> do 
                        content <- reader h
                        ByteString.hPut wH (GZip.compress $ filterFile content)
                )))
    putStrLn "Done."

whichReader file
    | isGz file = readGziped
    | otherwise = readSimple
    where 
    readSimple = ByteString.hGetContents
    readGziped =  fmap GZip.decompress . readSimple
