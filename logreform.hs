#!/usr/bin/env stack
{- stack 
  --resolver lts-14.9
  --install-ghc
  runghc
  --package bytestring
  --package stringsearch
  --package zlib
-} 

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad (forM_)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString.Lazy.Search.KMP as Lazy
import System.CPUTime (getCPUTime)
import System.Directory (listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath.Windows (dropTrailingPathSeparator, pathSeparator, takeExtension)
import System.IO
import Text.Printf

basePath dir = (dropTrailingPathSeparator dir)

withBase dir = (++) ((basePath dir) ++ [pathSeparator])

getReaderByFileExt :: FilePath -> Handle -> IO Lazy.ByteString
getReaderByFileExt file
  | isGz file = readGziped
  | otherwise = readSimple
  where
    readSimple = Lazy.hGetContents
    readGziped = fmap GZip.decompress . readSimple
    isGz file = takeExtension file == ".gz"

patterns :: [Strict.ByteString]
patterns = ["TLS connection established from", " reject:", "client=", "warning: header Subject"]

filterLines patterns = filter match
  where
    match x = any ($ x) filterPatterns
    filterPatterns = map (\pat v -> not . null $ Lazy.indices pat v) patterns

grepLog :: [String] -> [Strict.ByteString] -> FilePath -> IO ()
grepLog files patterns out = do
  withFile out WriteMode $ \wH ->
    forM_ files $ \file ->
      let reader = getReaderByFileExt file
      in withFile file ReadMode $ \h -> do
        content <- reader h
        Lazy.hPut wH (GZip.compress $ filterContent content)
  where
    filterContent = Lazy.unlines . (filterLines patterns) . Lazy.lines


main :: IO ()
main = do
  putStrLn "Starting..."
  [dir, toFile] <- getArgs
  printf "Path: %s -> %s \n" dir toFile
  files <- fmap (reverse . map (withBase dir)) $ listDirectory dir
  printf "TotalFiles: %i \n" (length files)
  grepLog files patterns toFile
  putStrLn "Done."
