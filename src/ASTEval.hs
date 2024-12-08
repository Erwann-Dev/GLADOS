{-# LANGUAGE LambdaCase #-}

module ASTEval (
  State (..),
  eval,
) where

import AST
import Control.Applicative

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

set :: a -> State a ()
set x = State $ \_ -> Right ((), x)

get :: State a a
get = State $ \m -> Right (m, m)

eval :: Expr -> Env -> State Env Value
eval (Number n) _ = return $ NumVal n
eval (Boolean b) _ = return $ BoolVal b
eval (List (expr : exprs)) env =
  eval expr env >>= \case
    (Closure{}) -> eval (Apply expr exprs) env
    _ -> ListVal <$> mapM (`eval` env) (expr : exprs)
eval (List xs) env = ListVal <$> mapM (`eval` env) xs
eval (Add e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 + n2)
    _ -> fail "(+): type error"
eval (Sub e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 - n2)
    _ -> fail "(-): type error"
eval (Mul e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 * n2)
    _ -> fail "(*): type error"
eval (Div e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) ->
      if n2 == 0
        then fail "Division by zero"
        else return $ NumVal (n1 `div` n2)
    _ -> fail "Div: type error"
eval (Eq e1 e2) env = BoolVal <$> ((==) <$> eval e1 env <*> eval e2 env)
eval (Neq e1 e2) env = BoolVal <$> ((/=) <$> eval e1 env <*> eval e2 env)
eval (Gt e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ BoolVal (n1 > n2)
    _ -> fail "(>): type error"
eval (Lt e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ BoolVal (n1 < n2)
    _ -> fail "(<): type error"
eval (Lam ids e) env = return $ Closure ids e env
eval (If g e1 e2) env =
  eval g env >>= \case
    BoolVal True -> eval e1 env
    BoolVal False -> eval e2 env
    _ -> fail "If: type error"
eval (Apply f xs) env = do
  f' <- eval f env
  xs' <- mapM (`eval` env) xs
  apply f' xs'
eval (Var str) env = do
  mem <- get
  case lookup str env <|> lookup str mem of
    Just x -> return x
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (Define str e) env = do
  e' <- eval e env
  mem <- get
  set ((str, e') : mem)
  return Null

apply :: Value -> [Value] -> State Env Value
apply (Closure ids e env) xs
  | length ids == length xs = eval e (zip ids xs ++ env)
  | otherwise = fail "Arguments mismatch"
apply _ _ = fail "Expected closure"
