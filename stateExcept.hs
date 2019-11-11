{-# LANGUAGE FlexibleContexts #-}
module StateExcept where
    
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable


limited :: (MonadState s m, MonadError Int m) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

-- |
-- >>> runLimited1 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- (Left 2,3)
-- >>> runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- (Right [(),(),(),()],4)
runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

-- |
-- >>> runLimited2 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- Left 2
-- >>> runLimited2 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- Right ([(),(),(),()],4)
runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s


run1 :: ExceptT e (State s) a -> s -> (Either e a, s)
run1 = runState . runExceptT
run2 :: StateT s (Except e) a -> s -> Either e (a, s)
run2 = (runExcept .) . runStateT