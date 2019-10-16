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
    liftPrint res
    put res
    return n
        where
            liftPrint = lift . putStrLn . show 
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
            

instance (Functor m) => Functor (Arr2T e1 e2 m) where
    fmap f n = Arr2T $ (fmap f .) . getArr2T n


instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
   fmap f n = Arr3T $ ((fmap f .).) . getArr3T n

-- | Stepic Test Applicative instance for Arr2T
-- >>> let a2l = Arr2T $ \e1 e2 -> [e1,e2]
-- >>> let a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
-- >>> getArr2T (a2fl <*> a2l) 2 10
-- [22,30,7,7]
instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
    pure = Arr2T . const2 . pure
    f <*> v = Arr2T $ \e1 e2 -> getArr2T f e1 e2 <*> getArr2T v e1 e2 
    
-- | Stepic Test Applicative instance for Arr3T
-- >>> let a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
-- >>> let a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]
-- >>> getArr3T (a3fl <*> a3l) 3 5 7
-- [8,10,10,12]
instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
    pure = Arr3T . const3 . pure
    f <*> v = Arr3T $ \e1 e2 e3 -> getArr3T f e1 e2 e3 <*> getArr3T v e1 e2 e3


-- | Stepic Test Monad instance for Arr2T
-- >>> let a2l = Arr2T $ \e1 e2 -> [e1,e2]
-- >>> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
-- [6,8,8,10]
instance (Monad m) => Monad (Arr2T e1 e2 m) where
    m >>= a2 = Arr2T $ \e1 e2 -> do
        v <- getArr2T m e1 e2 
        getArr2T (a2 v) e1 e2

-- | Stepic Test Monad instance for Arr3T
-- >>> let a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
-- >>> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
-- Just 81
instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
    m >>= a3 = Arr3T $ \e1 e2 e3 -> do
        v <- getArr3T m e1 e2 e3
        getArr3T (a3 v) e1 e2 e3
    fail = Arr3T . const3 . fail



const2 :: a -> b -> c -> a
const2 = const . const

const3 :: a -> b -> c -> d -> a
const3 = const . const . const

-- | MonadTrance instance for Arr2t
-- >>> let a2l = Arr2T $ \e1 e2 -> [e1,e2]
-- >>> getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4
-- [13,23,33,14,24,34]
instance MonadTrans (Arr2T e1 e2) where
    lift m = Arr2T $ \_ _ -> m

-- | Impl of ReaderT interface `asks` for Arr2T
-- >>> getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'
-- ('A','B',('A','B'))
asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ (return .). f