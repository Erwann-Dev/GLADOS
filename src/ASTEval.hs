module ASTEval (
  eval,
  Value (..),
  Env,
  State (..),
  initialState,
  InterpreterState (..),
)
where

import AST
import Control.Applicative
import Control.Monad

newtype State s a = State {runState :: s -> Either String (a, s)}

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

getEnvs :: State InterpreterState [Env]
getEnvs = envStack <$> get

setEnvs :: [Env] -> State InterpreterState ()
setEnvs envs = do
  state <- get
  set state{envStack = envs}

insertInEnv :: Symbol -> Value -> [Env] -> [Env]
insertInEnv str val [] = [[(str, val)]]
insertInEnv str val (env : envs) = case lookup str env of
  Just _ -> ((str, val) : filter ((/= str) . fst) env) : envs
  Nothing -> env : insertInEnv str val envs

addToPrintBuffer :: Value -> State InterpreterState ()
addToPrintBuffer val = do
  state <- get
  set state{printBuffer = printBuffer state ++ [val]}

type Env = [(Symbol, Value)]

data InterpreterState = InterpreterState
  { envStack :: [Env] -- Stack of environments
  , printBuffer :: [Value] -- Buffer of values to print
  }

initialState :: InterpreterState
initialState =
  InterpreterState
    { envStack = [[]]
    , printBuffer = []
    }

data Value
  = NumVal Int
  | FloatVal Float
  | ListVal [Value]
  | Closure [(Type, Symbol)] Node Env
  | Null
  deriving (Show, Eq)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

eval :: Node -> Env -> State InterpreterState Value
eval (IntV n) _ = return $ NumVal n
eval (FloatV b) _ = return $ FloatVal b
eval (ArrayV xs) env = ListVal <$> mapM (`eval` env) xs
eval (AddOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 + n2)
    (FloatVal n1, NumVal n2) -> return $ FloatVal (n1 + fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal (n1 + round n2)
    (FloatVal n1, FloatVal n2) -> return $ FloatVal (n1 + n2)
    _ -> fail "(+): type error"
eval (SubOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 - n2)
    (FloatVal n1, NumVal n2) -> return $ FloatVal (n1 - fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal (n1 - round n2)
    (FloatVal n1, FloatVal n2) -> return $ FloatVal (n1 - n2)
    _ -> fail "(-): type error"
eval (MulOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 * n2)
    (FloatVal n1, NumVal n2) -> return $ FloatVal (n1 * fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal (n1 * round n2)
    (FloatVal n1, FloatVal n2) -> return $ FloatVal (n1 * n2)
    _ -> fail "(*): type error"
eval (DivOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (_, NumVal 0) -> fail "Division by zero"
    (_, FloatVal 0) -> fail "Division by zero"
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 `div` n2)
    (FloatVal n1, NumVal n2) -> return $ FloatVal (n1 / fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal (n1 `div` round n2)
    (FloatVal n1, FloatVal n2) -> return $ FloatVal (n1 / n2)
    _ -> fail "(/): type error"
eval (AddEqOp str e) env = eval (VarAssign str $ AddOp (Identifier str) e) env
eval (SubEqOp str e) env = eval (VarAssign str $ SubOp (Identifier str) e) env
eval (MulEqOp str e) env = eval (VarAssign str $ MulOp (Identifier str) e) env
eval (DivEqOp str e) env = eval (VarAssign str $ DivOp (Identifier str) e) env
eval (NotOp e) env = do
  v <- eval e env
  case v of
    NumVal n -> return $ NumVal $ boolToInt (n == 0)
    _ -> fail "not: type error"
eval (AndOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 /= 0 && n2 /= 0)
    _ -> fail "and: type error"
eval (OrOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 /= 0 || n2 /= 0)
    _ -> fail "or: type error"
eval (EqOp e1 e2) env = NumVal . boolToInt <$> ((==) <$> eval e1 env <*> eval e2 env)
eval (NeqOp e1 e2) env = eval (NotOp (EqOp e1 e2)) env
eval (GtOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 > n2)
    (FloatVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 > fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (fromIntegral n1 > n2)
    (FloatVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (n1 > n2)
    _ -> fail "(>): type error"
eval (LtOp e1 e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 < n2)
    (FloatVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 < fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (fromIntegral n1 < n2)
    (FloatVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (n1 < n2)
    _ -> fail "(<): type error"
eval (LeqOp e1 e2) env = eval (OrOp (LtOp e1 e2) (EqOp e1 e2)) env
eval (GeqOp e1 e2) env = eval (OrOp (GtOp e1 e2) (EqOp e1 e2)) env
eval (Identifier str) env = do
  mem <- getEnvs
  case lookup str env <|> lookupMem str mem of
    Just x -> return x
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (VarDef str _type e) env = do
  e' <- eval e env
  mem <- getEnvs
  case lookup str (head mem) of
    Just _ -> fail $ "variable " ++ str ++ " is already bound."
    Nothing -> setEnvs (((str, e') : head mem) : tail mem) >> return Null
eval (VarAssign str e) env = do
  e' <- eval e env
  mem <- getEnvs
  case lookupMem str mem of
    Just _ -> setEnvs (insertInEnv str e' mem) >> return Null
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (Block [] _) _env = do
  mem <- getEnvs
  setEnvs (tail mem)
  return Null
eval (Block (Return e : _) _) env = do
  e' <- eval e env
  mem <- getEnvs
  setEnvs (tail mem)
  return e'
eval (Block (e : es) isBlockStart) env = do
  when isBlockStart (getEnvs >>= \mem -> setEnvs ([] : mem))
  eval e env >> eval (Block es False) env
eval (Return e) env = do
  mem <- getEnvs
  if length mem > 1
    then eval e env
    else fail "Return outside of block"
eval (If cond e1 e2) env = do
  evaledCond <- eval cond env
  case evaledCond of
    NumVal i | i /= 0 -> eval e1 env
    NumVal 0 -> case e2 of
      (Just e2') -> eval e2' env
      _ -> return Null
    _ -> fail "If: type error"
eval (While cond e) env = do
  evaledCond <- eval cond env
  case evaledCond of
    NumVal i | i /= 0 -> eval e env >> eval (While cond e) env
    NumVal 0 -> return Null
    _ -> fail "While: type error"
eval (For base cond inc body) env = do
  _ <- case base of
    Nothing -> return Null
    Just e -> eval e env
  evaledCond <- eval cond env
  case evaledCond of
    NumVal i
      | i /= 0 ->
          eval body env
            >> ( case inc of
                  Nothing -> return Null
                  (Just e) -> eval e env
               )
            >> eval (For Nothing cond inc body) env
    NumVal 0 -> return Null
    _ -> fail "For: type error"
eval (FunctionDeclaration _type str args body) env = do
  mem <- getEnvs
  case lookupMem str mem of
    Just _ -> fail $ "variable " ++ str ++ " is already bound."
    Nothing -> setEnvs (((str, Closure args body env) : head mem) : tail mem) >> return Null
eval (FunctionCall id_ args) env = do
  id' <- eval id_ env
  args' <- mapM (`eval` env) args
  apply id' args'
eval (Print e) env = do
  value <- eval e env
  addToPrintBuffer value
  return Null
eval _ _ = fail "Not implemented"

lookupMem :: Symbol -> [Env] -> Maybe Value
lookupMem _ [] = Nothing
lookupMem str (env : envs) = lookup str env <|> lookupMem str envs

apply :: Value -> [Value] -> State InterpreterState Value
apply (Closure ids e env) xs
  | length ids == length xs = eval e (zip (map snd ids) xs ++ env)
  | otherwise = fail "Arguments mismatch"
apply _ _ = fail "Expected closure"
