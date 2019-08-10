-- Random CODE generation for https://laysmusic.ru

module Main where

import System.Random
import Control.Monad (forM_, foldM)
import Control.Concurrent (threadDelay)


-- Get a random item out of a list
selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)


-- Alternate way of selecting a random elements from a passed in range
getRandomElementsFromRange :: Int -> [a] -> IO [a]
getRandomElementsFromRange _ [] = error "Range is empty"
getRandomElementsFromRange size range = do
  -- Create the random number generator
  g <- newStdGen
  -- Get a list of random integers which will map the random elements to choose
  let randomInts = take size $ randomRs (0, length range - 1) g
  return $ map (\n -> range !! n) randomInts

source = ['0'..'9'] ++ ['a' .. 'z']

getLaysRandomCode = getRandomElementsFromRange 6 source
oneSecond = 1000000 

pushLaysCode :: String -> IO (Either String String)
pushLaysCode code = do 
    threadDelay oneSecond
    putStrLn code
    selectRandomElement [Right "OK", Left "testErr"]

pushLaysCodes :: Int -> IO ()
pushLaysCodes gTryes = pushLaysCodes' gTryes "" []
    where
        pushLaysCodes' :: Int -> String -> [String] -> IO ()
        pushLaysCodes' 0 _ _ = return ()
        pushLaysCodes' n "" prefixes = getLaysRandomCode >>= \code -> pushLaysCodes' n code prefixes
        pushLaysCodes' n code [] = pushLaysCodes' n code ["cl", "sc", "su", "sk", "sc", "sp", "co"]
        pushLaysCodes' tryes code (prefix:left) = do
            resp <- pushLaysCode (prefix ++ code)
            case resp of
                (Right _) -> pushLaysCodes gTryes
                (Left err) -> print err >> pushLaysCodes' (tryes -1) code left


readGoodCodes :: IO [String]
readGoodCodes = undefined

main :: IO ()
main = do
    goodCodes <- readGoodCodes
    forM_ goodCodes $ \goodCode -> do
        putStrLn $ "Get new goodCode " ++ goodCode
        -- after 10 tryes you'll be banned
        pushLaysCodes 8
        -- try send good code to refresh ban counter
        putStrLn $ "Random tryes left. Tryed good code `" ++ goodCode ++ "`"
        resp <- pushLaysCode goodCode
        case resp of 
            (Right ansv) -> putStrLn "GOOD! Refresh counter" >> return ()
            (Left ansv)  -> error $ "Oooops! Good code failed! " ++ ansv
    
    putStrLn "No codes left"