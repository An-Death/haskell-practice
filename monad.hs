module DemoMonad where

import Control.Monad (ap, liftM)

{-
Class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    ...
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