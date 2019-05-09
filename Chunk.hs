{-# LANGUAGE OverloadedStrings #-}
module Chunk where

import System.IO
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)


data Chunk = Chunk { chunk :: String } 
            | LineEnd {chunk :: String, remainder :: String}
    deriving (Show)

newtype Iter = Iter {runIter:: B8.ByteString -> IterResult}

data IterResult = HaveLine {line :: String, residual :: String}
                | NeedChunk Iter

instance Show IterResult where
    show (HaveLine l r) = "HaveLine: " ++ l ++ "|" ++ r
    show (NeedChunk _)  = "NeedChunk"

parseChunk chunk = 
    if rightS == B8.pack ""
        then Chunk (toS leftS)
        else LineEnd (toS leftS) ((toS . B8.tail) rightS)
    where
        (leftS, rightS) = B8.break (== '\n') chunk

toS = map (chr . fromEnum) . B.unpack


chunkIter :: Iter
chunkIter = Iter (go "")
    where
    go :: String -> B8.ByteString -> IterResult
    go acc chunk = 
        case (parseChunk chunk) of
            (Chunk chunk') -> NeedChunk (Iter (go (acc ++ chunk')))
            (LineEnd chunk' residual') -> HaveLine (acc ++ chunk') residual'

withOpenFile :: FilePath -> Iter -> (Handle -> IO B8.ByteString) -> (String -> IO ()) -> IO () 
withOpenFile path initIter reader processor = 
    withFile path ReadMode $ \h ->
        let 
        go iter = do
            isEOF <- hIsEOF h
            if isEOF 
                then return ()
                else do 
                    chunk <- reader h
                    check $ runIter iter chunk
    
        check (NeedChunk iterNext) = go iterNext
        check (HaveLine line residual) = do
            processor line
            check $ runIter initIter (B8.pack residual)
        in go initIter

