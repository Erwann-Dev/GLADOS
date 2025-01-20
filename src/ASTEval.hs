{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

insertInEnv :: Symbol -> Value -> Bool -> [Env] -> [Env]
insertInEnv str val isMut [] = [[(str, (val, isMut))]]
insertInEnv str val isMut (env : envs) = case lookup str env of
  Just _ -> ((str, (val, isMut)) : filter ((/= str) . fst) env) : envs
  Nothing -> env : insertInEnv str val isMut envs

addToPrintBuffer :: Value -> State InterpreterState ()
addToPrintBuffer val = do
  state <- get
  set state{printBuffer = printBuffer state ++ [val]}

type Env = [(Symbol, (Value, Bool))]

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
  | Error String
  | EarlyReturn Value
  deriving (Show, Eq)

-- Helper type class for operations between values
class NumericOp a where
  numericOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> a -> a -> Value

instance NumericOp Value where
  numericOp intOp floatOp v1 v2 = case (v1, v2) of
    (NumVal n1, NumVal n2) -> NumVal (intOp n1 n2)
    (FloatVal f1, FloatVal f2) -> FloatVal (floatOp f1 f2)
    (NumVal n1, FloatVal f2) -> FloatVal (floatOp (fromIntegral n1) f2)
    (FloatVal f1, NumVal n2) -> FloatVal (floatOp f1 (fromIntegral n2))
    _ -> Error "type error" -- Using error here as we'll wrap it in Either later

-- Helper functions for common operations
evalNumericOp :: (Value -> Value -> Value) -> Node -> Node -> Env -> State InterpreterState Value
evalNumericOp op e1 e2 env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  case op v1 v2 of
    (Error str) -> fail $ show str
    result -> return result

add :: Value -> Value -> Value
add = numericOp (+) (+)

sub :: Value -> Value -> Value
sub = numericOp (-) (-)

mul :: Value -> Value -> Value
mul = numericOp (*) (*)

div_ :: Value -> Value -> Value
div_ _ (NumVal 0) = Error "Division by zero"
div_ _ (FloatVal 0.0) = Error "Division by zero"
div_ v1 v2 = numericOp div (/) v1 v2

mod_ :: Value -> Value -> Value
mod_ _ (NumVal 0) = Error "Division by zero"
mod_ _ (FloatVal _) = Error "Modulo by Floating point value"
mod_ (FloatVal _) _ = Error "Modulo by Floating point value"
mod_ v1 v2 = numericOp mod const v1 v2

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

eval :: Node -> Env -> State InterpreterState Value
eval (IntV n) _ = return $ NumVal n
eval (FloatV b) _ = return $ FloatVal b
eval (ArrayV xs) env = ListVal <$> mapM (`eval` env) xs
eval (AddOp e1 e2) env = evalNumericOp add e1 e2 env
eval (SubOp e1 e2) env = evalNumericOp sub e1 e2 env
eval (MulOp e1 e2) env = evalNumericOp mul e1 e2 env
eval (DivOp e1 e2) env = evalNumericOp div_ e1 e2 env
eval (ModOp e1 e2) env = evalNumericOp mod_ e1 e2 env
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
    Just (x, _) -> return x
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (VarDef str (Type _ isMut) e) env = do
  e' <- eval e env
  mem <- getEnvs
  case lookup str (head mem) of
    Just _ -> fail $ "variable " ++ str ++ " is already bound."
    Nothing -> setEnvs (((str, (e', isMut)) : head mem) : tail mem) >> return Null
eval (VarAssign str e) env = do
  e' <- eval e env
  mem <- getEnvs
  case lookupMem str mem of
    Just (_, False) -> fail $ "variable " ++ str ++ " is not mutable."
    Just _ -> setEnvs (insertInEnv str e' True mem) >> return Null
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
  eval e env >>= \case
    (EarlyReturn v) -> do
      getEnvs >>= setEnvs . tail
      return v
    _ -> eval (Block es False) env
eval (ConditionalBody []) _env = return Null
eval (ConditionalBody (Return e : _)) env = eval e env >>= \e' -> pure $ EarlyReturn e'
eval (ConditionalBody (e : es)) env =
  eval e env >> eval (ConditionalBody es) env
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
    Nothing -> setEnvs (((str, (Closure args body env, False)) : head mem) : tail mem) >> return Null
eval (FunctionCall id_ args) env = do
  id' <- eval id_ env
  args' <- mapM (`eval` env) args
  apply id' args'
eval (Print e) env = do
  value <- eval e env
  addToPrintBuffer value
  return Null
eval (EnumDeclaration _ _) _ = fail "Enums are not supported"
eval (StructDeclaration _ _) _ = fail "Structs are not supported"
eval (StructInitialization _ _) _ = fail "Structs are not supported"
eval (EnumElement _ _) _ = fail "Enums are not supported"
eval (StructElement _ _) _ = fail "Structs are not supported"
eval (CastToType _ _) _ = fail "Casting is not supported"
eval (CastToIdentifier _ _) _ = fail "Casting is not supported"
eval (SizeofType _) _ = fail "Sizeof is not supported"
eval (SizeofExpr _) _ = fail "Sizeof is not supported"
eval (Reference _) _ = fail "References are not supported"
eval (Dereference _) _ = fail "References are not supported"
eval (ArrayAccess _ _) _ = fail "Array access is not supported"
eval (Syscall _) _ = fail "Syscalls are not supported"

lookupMem :: Symbol -> [Env] -> Maybe (Value, Bool)
lookupMem _ [] = Nothing
lookupMem str (env : envs) = lookup str env <|> lookupMem str envs

apply :: Value -> [Value] -> State InterpreterState Value
apply (Closure ids e env) xs
  | length ids == length xs = eval e (linkedParams ++ env)
  | otherwise = fail "Arguments mismatch"
 where
  linkedParams = [(sym, (val, isMut)) | ((Type _ isMut, sym), val) <- zip ids xs]
apply _ _ = fail "Expected closure"
