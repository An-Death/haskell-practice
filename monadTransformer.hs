{-# LANGUAGE DeriveGeneric #-}
module Demo where

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus (mplus, mzero), ap, guard, liftM, msum)
import Text.Read (readEither)

newtype Except e a = Except {runExcept :: Either e a}
  deriving (Show)

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where

  pure = return

  (<*>) = ap

instance Monad (Except e) where

  --  return :: a -> Except e a
  return a = Except (Right a)

  --  (>>=) :: Except e a -> (a -> Except e b) -> Except e b
  m >>= k =
    case runExcept m of
      Left e -> Except $ Left e
      Right a -> k a

instance Monoid e => Alternative (Except e) where

  empty = mzero

  (<|>) = mplus

instance Monoid e => MonadPlus (Except e) where

  mzero = Except (Left mempty)

  Except x `mplus` Except y = Except
    $ case x of
      Left e -> either (Left . mappend e) Right y
      r -> r

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
catchE m h =
  case runExcept m of
    Left e -> h e
    Right a -> except (Right a)

{-
do {action1; action2; action3 } `catchE` handler

Law:
catchE h (throwE e) == h e

-}
data DivByZero = ErrZero String | ErrOther
  deriving (Eq, Show)

(/?) :: Double -> Double -> Except DivByZero Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByZero String
example0 x y = action `catchE` handler
  where
    action = do
      q <- x /? y
      return $ show q
    handler = \err -> return $ show err

data ReadError = EmptyInput | NoParse String
  deriving (Show)

-- | Stepic Task 3.1.8
--
-- >>> runExcept (tryRead "5" :: Except ReadError Int)
-- Right 5
-- >>> runExcept (tryRead "5" :: Except ReadError Double)
-- Right 5.0
-- >>> runExcept (tryRead "5zzz" :: Except ReadError Int)
-- Left (NoParse "5zzz")
-- >>> runExcept (tryRead "" :: Except ReadError (Bool, ()))
-- Left EmptyInput
-- >>> runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
-- Left (NoParse "wrong")
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)

data SumError = SumError Int ReadError
  deriving (Show)

-- | Stepic Task 3.1.9
--
-- >>> runExcept $ trySum ["10", "20", "30"]
-- Right 60
-- >>> runExcept $ trySum ["10", "20", ""]
-- Left (SumError 3 EmptyInput)
-- >>> runExcept $ trySum ["10", "two", "30"]
-- Left (SumError 2 (NoParse "two"))
trySum :: [String] -> Except SumError Integer
trySum = fmap sum . traverse f . enumirate
  where
    enumirate = zip [1 ..]
    f (pos, s) = SumError pos `withExcept` tryRead s

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except e) = case e of
  (Left e) -> Except (Left (f e))
  (Right a) -> Except (Right a)

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

newtype SimpleError = Simple {getSimple :: String}
  deriving (Eq, Show)

instance Semigroup SimpleError

instance Monoid SimpleError where

  mempty = Simple ""

  Simple l `mappend` Simple r = Simple $ l ++ r

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
lie2se (ErrNegativeIndex) = Simple "[negative index]"

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs i
  | i < 0 = throwE ErrNegativeIndex
  | otherwise = helper xs i
  where
    helper [] n = throwE $ ErrIndexTooLarge i
    helper (x : _) 0 = except $ Right x
    helper (_ : xs) i = helper xs (i - 1)

newtype Validate e a = Validate {getValidate :: Either [e] a}
  deriving (Show)

instance Functor (Validate e) where
  fmap f = Validate . fmap f . getValidate

instance Applicative (Validate e) where
  pure = Validate . pure
  
  Validate (Right f) <*> vx = f <$> vx
  vf <*> Validate (Right x) = ($ x) <$> vf
  Validate (Left es1) <*> Validate (Left es2) = Validate $ Left (es1 `mappend` es2)

collectE :: Except e a -> Validate e a
collectE = either (Validate . Left . (: [])) pure . runExcept

validateSum :: [String] -> Validate SumError Integer
validateSum = fmap sum . traverse f . enumirate
  where
    enumirate = zip [1 ..]
    f (i, s) = collectE $ withExcept (SumError i) $ tryRead s

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r)

instance Applicative (Cont r)

instance Monad (Cont r) where
  -- return :: a -> Cont r a
  return x = Cont $ \c -> c x
  -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

evalCont :: Cont r r -> r
evalCont m = runCont m id

square' :: Int -> Cont r Int
square' x = return $ x ^ 2

add' :: Int -> Int -> Cont t Int
add' x y = return $ x + y

type Checkpointed a = (a -> Cont a a) -> Cont a a

-- | How to reimplement Futures
runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed pred chekpoint = runCont check id
  where
    check =
      chekpoint
        ( \a ->
            Cont $ \c ->
              let next = c a
               in if pred next
                    then next
                    else a
          )

-- | Stepik 3.2.9
--
-- >>> runCheckpointed (< 100) $ addTens 1
-- 31
-- >>> runCheckpointed  (< 30) $ addTens 1
-- 21
-- >>> runCheckpointed  (< 20) $ addTens 1
-- 11
-- >>> runCheckpointed  (< 10) $ addTens 1
-- 1
addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2 {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3 {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4 {- x4 = x1 + 30 -}

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (e -> r) -> r}

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

instance Monad (FailCont r e) where
  -- return :: a -> FailCont r e a
  return x = FailCont $ \c _ -> c x
  -- (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
  FailCont v >>= k = FailCont $ \c ce -> v (\c' -> runFailCont (k c') c ce) ce

toFailCont :: Except e a -> FailCont r e a
toFailCont exc = FailCont $ \c ce -> either ce c (runExcept exc)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

-- | Stepic 3.2.10
--
-- >>> evalFailCont $ addInts "15" "12"
-- Right 27
-- >>> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
-- Oops: EmptyInput
addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

--                (a -> r) -> (e -> r) -> r
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ 
    \cont contE -> 
        runFailCont (f $ \a -> FailCont $ \_ _ -> cont a) cont contE
