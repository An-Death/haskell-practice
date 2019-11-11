{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, UndecidableInstances  #-}
module Coroutine where
--https://stepik.org/lesson/45331/step/6?unit=23645

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

import Control.Applicative
import Control.Monad.Cont


newtype CoroutineT' r m a = CoroutineT' { runCoroutineT' :: ContT r (StateT [CoroutineT' r m ()] m) a }
 deriving (Functor,Applicative,Monad,MonadCont,MonadIO)

type CoroutineT m a = CoroutineT' () m a

instance MonadTrans (CoroutineT' r) where
    lift = CoroutineT' . lift .lift  

instance MonadWriter w m => MonadWriter w (CoroutineT' r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = undefined


runCoroutineT :: Monad m => CoroutineT' r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

-- | 
-- >>> execWriter (runCoroutines coroutine3 coroutine4)
-- "1ab2cd"
runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines c1 c2 = runCoroutineT $ do
    fork c1
    fork c2

-- Used to manipulate the coroutine queue.
getCCs :: Monad m => CoroutineT' r m [CoroutineT' r m ()]
getCCs = CoroutineT' $ lift get

putCCs :: Monad m => [CoroutineT' r m ()] -> CoroutineT' r m ()
putCCs = CoroutineT' . lift . put

-- Exhaust passes control to suspended coroutines repeatedly until there isn't any left.
exhaust :: Monad m => CoroutineT' r m ()
exhaust = do
    exhausted <- null <$> getCCs
    if not exhausted
        then yield >> exhaust
        else return ()
  
-- Pop and push coroutines to the queue.
dequeue :: Monad m => CoroutineT' r m ()
dequeue = do
    current_ccs <- getCCs
    case current_ccs of
        [] -> return ()
        (p:ps) -> do
            putCCs ps
            p

queue :: Monad m => CoroutineT' r m () -> CoroutineT' r m ()
queue p = do
    ccs <- getCCs
    putCCs (ccs++[p])
    
-- The interface.
yield :: Monad m => CoroutineT' r m ()
yield = callCC $ \k -> do
    queue (k ())
    dequeue

fork :: Monad m => CoroutineT' r m () -> CoroutineT' r m ()
fork p = callCC $ \k -> do
    queue (k ())
    p
    dequeue

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield