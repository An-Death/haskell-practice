{-# LANGUAGE FlexibleContexts #-}
module DemoMonad where

import Data.List (isInfixOf)
import Data.Char (isDigit)
import Control.Monad (ap, liftM)
import Control.Monad.Writer
import System.CPUTime (getCPUTime)
import System.Directory (getDirectoryContents)
import Text.Printf
{-
Class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    -- x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail s = error s

-}

data Log a = Log [String] a    
    deriving (Show, Eq)

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

instance Functor Log where
    fmap = liftM
  
instance Applicative Log where
    pure = return
    (<*>) = ap


toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f
    
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log [msg1, msg2] result
  where
    Log [msg1] x'     = f x
    Log [msg2] result = g x'

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

returnLog :: a -> Log a
returnLog = Log [] 


bindLog :: Log a -> (a -> Log b) -> Log b
(Log msg's v) `bindLog` f = Log (msg's++msg's2) result 
    where
        Log msg's2 result = f v

toKleisli :: Monad m => (a->b) -> a -> m b
toKleisli f = return . f 

mult2Log = toLogger (* 2) "multiplied by 2"
add1Log = toLogger (+1) "added one"

-- (Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log) == (Log ["nothing done yet","added one","multiplied by 2"] 8)


newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Show)
{- 
class Monad m where 
    return :: a -> ma
    (>>=) :: m a -> (a -> m b) -> m b
-}

instance Monad Identity where
    return x = Identity x
    Identity x >>= k = k x
      
instance Functor Identity where
    fmap = liftM
    
instance Applicative Identity where
    pure = return
    (<*>) = ap

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

{-
first law of Monad's
return a >>= k == k a

second law of Monad's
m >>= return  == m

third 
m >>= k >>= k' == m >>= (\x -> k x >>= k')

-}

goWrap0 = 
    wrap'n'succ 3 >>=
    wrap'n'succ   >>=
    wrap'n'succ   >>=
    return

    
goWrap1 = 
    wrap'n'succ 3 >>= (\x -> 
    wrap'n'succ x >>= (\y -> 
    wrap'n'succ y >>= (\z -> 
    return z )))

goWrap2 = 
    wrap'n'succ 3 >>= (\x ->   -- x := succ 3;
    wrap'n'succ x >>= (\y ->   -- y := succ x;
    wrap'n'succ y >>= (\z ->   -- z := succ y;
    return (x, y, z) )))       -- return (x,y,z)

goWrap3 = 
    wrap'n'succ 3 >>= \x ->   
    wrap'n'succ x >>= \y ->   
    wrap'n'succ y >>  
    return (x + y)     

goWrap4 = 
    let i = 3 in  
        
    wrap'n'succ i >>= \x ->   
    wrap'n'succ x >>= \y ->   
    wrap'n'succ y >>  
    return (i, x + y)

goWrap5 = do
    let i = 3
    x <- wrap'n'succ i 
    y <- wrap'n'succ x
    wrap'n'succ y
    return (i, x+y)

--- Monad Maybe

type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [
    ("Bill", "John"),
    ("Ann", "John"),
    ("John", "Piter")
    ]
mothers = [
    ("Bill", "Jane"),
    ("Ann", "Jane"),
    ("John", "Alice"),
    ("Jane", "Dorothy"),
    ("Alice", "Mary")
    ]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

granmas :: Name -> Maybe (Name, Name)
granmas person = do
    m   <- getM person
    gmm <- getM m
    f   <- getF person
    gmf <- getM f
    return (gmm, gmf)


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x 
    | x == "+" = Just Plus
    | x == "-" = Just Minus
    | x == "(" = Just LeftBrace
    | x == ")" = Just RightBrace
    | all isDigit x = Just (Number (read x))
    | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words

type Board = Int
nextPositions x = [x-1,x+1]
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
    | n < 0     = []
    | otherwise = filter pred $ foldl (>>=) [b] (replicate n nextPositions)

    


-- list monad  equal [(x,y) | x <- [1,2,3],  y <- [1,2], x/=y]
lst' = do
    x <- [1,2,3]
    y <- [1,2]
    True <- return (x/=y)
    return (x,y)

lst'' = do
    x <- [1,2,3]
    y <- [1,2]
    if x/=y then "_" else []
    return (x,y)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x 
    | x <= 0     = []
    | otherwise  = do
        a <- [1..x]
        b <- [1..x]
        c <- [1..x]
        True <- return (a < b)
        True <- return (a*a + b*b == c*c)
        return (a,b,c)


removeFile x = putStrLn ("remove " ++ x)

main' :: IO ()
main' = do 
    putStr "Substring: "
    inp <- getLine
    if null inp 
        then putStrLn "Canceled"
    else do
        files <- getDirectoryContents "."
        mapM_ (\x -> sequence_ [putStrLn ("Removing file: "++x), removeFile x]) $ filter (isInfixOf inp) files


-- Monad Reader 
-- let's refresh our knowledge about Functor
{- 
    instance Functor ((->) e) where  -- e - is Enviroment
        fmap g h = g . h
        
        -- fmap :: (a -> b) -> f a -> f b           -- we could replace f to (e -> *)
        -- fmap :: (a -> b) -> (e -> a) -> (e -> b) -- that mean our function has some Enviroment context
        
        -- > :t fmap (^2) length
        -- fmap (^2) length :: Foldable t => t a -> Int
        -- here Enviroment is `t` == some list

    instance Monad ((->) e) where
        -- return :: a -> e -> a
        return x = \_ -> x          -- ignore Enviroment and return `x`. Is much trivial as possible
        (>>=) :: (e -> a) -> (a -> e -> b) -> e -> b
        m >>= k = \e -> k (m e) e
-}

safeHead = do
    b <- null
    if b then 
        return Nothing
    else do
        h <- head
        return $ Just h

-- extract Env 
safeHead' = do
    e <- id
    if null e then 
        return Nothing
    else
        return $ Just (head e)

newtype Reader r a = Reader { runReader :: (r -> a)}

instance Monad (Reader r) where
    return x = Reader $ \e -> x
    m >>= k = Reader $ \e ->
        let v = runReader m e
        in runReader (k v) e

instance Functor (Reader r) where
    fmap = liftM
    
instance Applicative (Reader r) where
    pure = return
    (<*>) = ap
        
ask :: Reader r r
ask = Reader id
-- Example
type User = String
type Password = String
type UserTable = [(User, Password)]

pwds :: UserTable
pwds = [("Bill", "123"), ("Ann", "querty"), ("John", "John$bad$boy")]

firstUser :: Reader UserTable User
firstUser = do
    e <- ask
    return $ fst (head e)

asks :: (r->a) -> Reader r a
asks = Reader

firstUserPassword :: Reader UserTable Password
firstUserPassword = asks (snd . head)

usersCount :: Reader UserTable Int
usersCount = asks length

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

localTest :: Reader UserTable (Int,Int)
localTest = do
    count1 <- usersCount
    count2 <- local (("Mike", "1"):) usersCount
    return (count1, count2)

reader :: (r -> a) -> Reader r a
reader f = do
    r <- ask
    return (f r)

usersWithBadPasswords :: Reader UserTable [User]
usersWithBadPasswords = asks (map fst . filter ((=="123456") . snd))


-- Monad Writer

-- newtype Writer w a = Writer {runWriter :: (a,w)}

-- writer :: (a,w) -> Writer w a
-- writer = Writer

-- execWriter :: Writer w a -> w
-- execWriter m = snd (runWriter m)

-- -- instance Functor (Writer w) where
-- --     fmap = liftM
    
-- -- instance Applicative (Writer w) where
-- --     pure = return
-- --     (<*>) = ap
    
-- -- instance (Monoid w) => Monad (Writer w) where
-- --     return x = Writer (x, mempty)
-- --     m >>= k = 
-- --         let (x,u) = runWriter m
-- --             (y,v) = runWriter $ k x
-- --         in Writer (y, u `mappend` v )

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
    let res = arg1 `op` arg2
    if abs res < 128 then
        return res
    else do
        tell "overflow"
        return 0


type Shopping = Writer ([String], Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = do 
    tell ([item], Sum cost)
    return ()
    
total :: Shopping -> Integer
total shop = x where 
    (_, (Sum x)) = execWriter shop

items :: Shopping -> [String]
items = fst . execWriter 