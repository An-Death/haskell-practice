module MonadCont where 

import MonadExcept
import Control.Monad (ap, liftM)

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



{- Stepik 3.2.3 CPS based DSL -}
-- |  Tests
--
-- >>> decode one hundred twenty three as a number
-- 123
-- >>> decode one hundred twenty one as a number
-- 121
-- >>> decode one hundred twenty as a number
-- 120
-- >>> decode one hundred as a number
-- 100
-- >>> decode three hundred as a number
-- 300
-- >>> decode two thousand seventeen as a number
-- 2017
decode :: (Int -> c) -> c
decode c = c 0

mulCPS :: Int -> Int -> (Int -> r) -> r
mulCPS x y c = c $ x * y

addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y c = c $ x + y

as :: Int -> (Int -> r) -> r
as x c = c x

a = as

number = id

one = addCPS 1
two = addCPS 2
three = addCPS 3
seventeen = addCPS 17
twenty = addCPS 20
thousand = mulCPS 1000
hundred = mulCPS 100


