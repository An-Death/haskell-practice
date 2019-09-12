module MonadTransformers where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Char (toUpper)
import Data.List (partition)

secondElem :: Reader [String] String
secondElem = do
    el2 <- asks (map toUpper . head . tail)
    return el2

logFirst :: [String] -> Writer String String
logFirst xs = do
    let
        el1 = head xs
        el2 = (map toUpper . head . tail) xs
    tell el1
    return el2


logFirstAndRetSecond :: ReaderT [String] -- Transformer
                        (Writer String)  -- inner monad
                        String           -- type of returns value
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head . tail)
    lift $ tell el1 
    return el2

-- | Stepik 3.3.3
-- 
-- >>> runReader (runWriterT logFirstAndRetSecondW) ["abc", "def", "ghi"]
-- ("DEF","abc")
logFirstAndRetSecondW :: WriterT String    -- Transformer
                        (Reader [String])  -- inner monad
                        String             -- type of returns value
logFirstAndRetSecondW = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head . tail)
    tell el1 
    return el2

-- | Stepik 3.3.4
--
-- >>> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
-- (([3,4,5,6,7],[0,1,2]),[8,9,10])
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 = filterM $ \x -> do
    when (p1 x) $        tell [x]
    when (p2 x) $ lift $ tell [x]
    return $ not (p1 x) && not (p2 x)


type MyRW = ReaderT [String] (Writer String) -- :k * -> *

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)

type MyRWT m = ReaderT [String] (WriterT String m) -- :k (* -> *) -> * -> *

runMyRWT :: Monad m => MyRWT m a -> [String] ->  m (a, String)
runMyRWT rwt e = runWriterT (runReaderT rwt e)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myAsk :: Monad m => MyRWT m [String]
myAsk = ask

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift


-- | Stepik 3.3.9 RWT
-- 
-- >>> runMyRWT logFirstAndRetSecondRWT ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")
logFirstAndRetSecondRWT :: MyRWT IO String
logFirstAndRetSecondRWT = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

-- | Stepik 3.3.10 
--
-- >>> runMyRWT veryComplexComputation ["abc","defg","hij"]
-- Nothing
-- >>> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")
veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do 
    xs <- myAsks $ partition isEven
    case xs of 
        (e1:e2:_ , o1:o2:_) ->  myTell (e1 ++  "," ++ o1)  >> 
                                return (map toUpper e2, map toUpper o2)
        _ -> myLift Nothing
    where 
        isEven = even . length


logFirstAndRetSecondMaybe :: MyRWT Maybe String
logFirstAndRetSecondMaybe = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

-- | Stepik 3.3.10 
--
-- >>> runMyRWT veryComplexComputation' ["abc","defg","hij"]
-- Nothing
-- >>> runMyRWT veryComplexComputation' ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")
veryComplexComputation' :: MyRWT Maybe (String, String)
veryComplexComputation' = do
    s1 <- myWithReader (filter $ even . length) logFirstAndRetSecondMaybe
    myTell ","
    s2 <- myWithReader (filter $ odd  . length) logFirstAndRetSecondMaybe
    return (s1, s2)

myWithReader :: Monad m => ([String] -> [String]) -> MyRWT m a -> MyRWT m a
myWithReader = withReaderT

-- | Stepik 3.3.11
-- 
-- >>> runEsSi (go 1 85 tickCollatz) 27
-- (Right (),82)
-- >>> runEsSi (go 1 80 tickCollatz) 27
-- (Left "Upper bound",82)
-- >>> runEsSi (forever $ go 1 1000 tickCollatz) 27
-- (Left "Upper bound",1186)
-- >>> runEsSi (forever $ go 1 10000 tickCollatz) 27
-- (Left "Lower bound",1)
tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT 

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go min_ max_ st = do 
    lift st  -- run 
    n <- lift $ get -- extract
    -- check
    validateBound (min_, max_) n

type Bounds = (Integer, Integer)
validateBound :: (Monad m) =>  Bounds -> Integer -> ExceptT [Char] m ()
validateBound (min_, max_) n = do
    when (n <= min_) $ throwE "Lower bound"
    when (n >= max_) $ throwE "Upper bound"

-- | Stepik 3.3.12
--
-- >>> runRiiEsSiT (forever $ goR tickCollatz') (1,200) 27
-- 82
-- 41
-- 124
-- 62
-- 31
-- 94
-- 47
-- 142
-- 71
-- 214
-- (Left "Upper bound",214)
type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                -> (Integer,Integer)  
                -> Integer 
                -> m (Either String a, Integer)
runRiiEsSiT r env = runStateT (runExceptT (runReaderT r env ))

goR :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
goR st = do
    lift . lift $ st
    n <- lift . lift $ get
    (min_, max_) <- ask
    lift $ validateBound (min_, max_) n

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n