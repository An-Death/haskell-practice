{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath.Windows (dropTrailingPathSeparator, pathSeparator, takeExtension)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf
import System.IO
import qualified Codec.Compression.GZip            as GZip
import qualified Data.ByteString.Lazy              as Lazy 
import qualified Data.ByteString.Lazy.Char8        as Lazy (lines, unlines)
import qualified Data.ByteString.Lazy.Search.KMP   as Lazy
import qualified Data.ByteString                   as Strict

basePath dir = (dropTrailingPathSeparator dir)
withBase dir = (++) ((basePath dir) ++ [pathSeparator])

isGz file = takeExtension file == ".gz"

whichReader file
    | isGz file = readGziped
    | otherwise = readSimple
    where 
    readSimple = Lazy.hGetContents
    readGziped =  fmap GZip.decompress . readSimple

patterns :: [Strict.ByteString]
patterns =  [" reject:", "client=", "warning: header Subject", "TLS connection established from"]
filterLines :: [Lazy.ByteString] ->  [Lazy.ByteString]
filterLines = filter match 
    where 
        match x = all ($ x) filterPatterns
        filterPatterns = map (\pat v -> not . null $ Lazy.indices pat v) patterns
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v 

filterFile :: Lazy.ByteString -> Lazy.ByteString
filterFile = Lazy.unlines . filterLines . Lazy.lines

main :: IO()
main =  do
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
                        Lazy.hPut wH (GZip.compress $ filterFile content)
                )))
    putStrLn "Done."

