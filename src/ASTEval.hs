module ASTEval where

import AST
import Control.Applicative
import Control.Monad

newtype State m a = State {runState :: m -> Either String (a, m)}

instance Functor (State m) where
  fmap f (State runstate) = State fun
   where
    fun state = do
      (returned, newState) <- runstate state
      return (f returned, newState)

instance Applicative (State m) where
  pure value = State $ \state -> Right (value, state)
  s1 <*> s2 = State $ \state -> do
    (f, newState) <- runState s1 state
    (returned, newState') <- runState s2 newState
    return (f returned, newState')

instance Monad (State m) where
  return = pure
  st >>= f = State $ \state -> do
    (returned, newState) <- runState st state
    let st' = f returned
    runState st' newState

instance MonadFail (State m) where
  fail = State . const . Left

eval :: Expr -> Env -> State Env Value
eval (Number n) _ = return $ NumVal n
eval (Boolean b) _ = return $ BoolVal b
eval _ _ = fail "Not implemented yet"
