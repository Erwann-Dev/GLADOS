{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

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

-- import Data.IntMap (member)
-- import Data.IntMap.Merge.Lazy (merge)
-- import Foreign.Marshal
-- import Foreign.Ptr
-- import Foreign.Storable

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

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

getIndexValue :: Int -> State InterpreterState (Value, Bool)
getIndexValue i = do
  mem <- getEnvs
  let flattenedMem = reverseList $ concat mem
  if i < 0 || i >= length flattenedMem
    then fail "Dereference: out of bounds"
    else return $ snd $ flattenedMem !! i

getSymbolIndex :: Symbol -> State InterpreterState Int
getSymbolIndex str = do
  mem <- getEnvs
  let flattenedMem = reverseList $ concat mem
  case lookup str flattenedMem of
    Just (_, _) -> return $ getSymbolIndex' str flattenedMem
    Nothing -> fail $ "variable " ++ str ++ " is not bound."

getSymbolIndex' :: Symbol -> Env -> Int
getSymbolIndex' _ [] = -1
getSymbolIndex' str ((sym, _) : env) = if str == sym then 0 else 1 + getSymbolIndex' str env

addToPrintBuffer :: Value -> State InterpreterState ()
addToPrintBuffer val = do
  state <- get
  set state{printBuffer = printBuffer state ++ [val]}

-- mallocedInt :: String -> Int -> IO (State InterpreterState Value)
-- mallocedInt identifier nb = do
--   intPtr <- malloc
--   poke intPtr nb
--   return $ State $ \state -> Right (PtrNumVal intPtr, state{intPointers = (identifier, intPtr) : intPointers state})
--
-- mallocedFloat :: String -> Float -> IO (State InterpreterState Value)
-- mallocedFloat identifier nb = do
--   floatPtr <- malloc
--   poke floatPtr nb
--   return $ State $ \state -> Right (PtrFloatVal floatPtr, state{floatPointers = (identifier, floatPtr) : floatPointers state})

type Env = [(Symbol, (Value, Bool))]

data InterpreterState = InterpreterState
  { envStack :: [Env] -- Stack of environments
  , printBuffer :: [Value] -- Buffer of values to print
  }

initialState :: InterpreterState
initialState =
  InterpreterState
    { envStack = [[("NULL", (Null, False))]]
    , printBuffer = []
    }

data Value
  = NumVal Int
  | FloatVal Float
  | ListVal [Value]
  | Closure [(Type, Symbol)] Node
  | Null
  | Error String
  | EarlyReturn Value
  deriving (Eq, Show)

-- instance Show Value where
--   show (NumVal n) = show n
--   show (FloatVal f) = show f
--   show (ListVal xs) = "[" ++ unwords (map show xs) ++ "]"
--   show (Closure _ _) = "<closure>"
--   show Null = "null"
--   show (Error str) = "Error: " ++ str
--   show (EarlyReturn v) = "Early return: " ++ show v

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
evalNumericOp :: (Value -> Value -> Value) -> Node -> Node -> State InterpreterState Value
evalNumericOp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
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

eval :: Node -> State InterpreterState Value
eval (IntV n) = return $ NumVal n
eval (FloatV b) = return $ FloatVal b
eval (ArrayV xs) = ListVal <$> mapM eval xs
eval (AddOp e1 e2) = evalNumericOp add e1 e2
eval (SubOp e1 e2) = evalNumericOp sub e1 e2
eval (MulOp e1 e2) = evalNumericOp mul e1 e2
eval (DivOp e1 e2) = evalNumericOp div_ e1 e2
eval (ModOp e1 e2) = evalNumericOp mod_ e1 e2
eval (AddEqOp str e) = eval (VarAssign str $ AddOp (Identifier str) e)
eval (SubEqOp str e) = eval (VarAssign str $ SubOp (Identifier str) e)
eval (MulEqOp str e) = eval (VarAssign str $ MulOp (Identifier str) e)
eval (DivEqOp str e) = eval (VarAssign str $ DivOp (Identifier str) e)
eval (NotOp e) = do
  v <- eval e
  case v of
    NumVal n -> return $ NumVal $ boolToInt (n == 0)
    _ -> fail "not: type error"
eval (AndOp e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 /= 0 && n2 /= 0)
    _ -> fail "and: type error"
eval (OrOp e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 /= 0 || n2 /= 0)
    _ -> fail "or: type error"
eval (EqOp e1 e2) = NumVal . boolToInt <$> ((==) <$> eval e1 <*> eval e2)
eval (NeqOp e1 e2) = eval (NotOp (EqOp e1 e2))
eval (GtOp e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 > n2)
    (FloatVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 > fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (fromIntegral n1 > n2)
    (FloatVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (n1 > n2)
    _ -> fail "(>): type error"
eval (LtOp e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 < n2)
    (FloatVal n1, NumVal n2) -> return $ NumVal $ boolToInt (n1 < fromIntegral n2)
    (NumVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (fromIntegral n1 < n2)
    (FloatVal n1, FloatVal n2) -> return $ NumVal $ boolToInt (n1 < n2)
    _ -> fail "(<): type error"
eval (LeqOp e1 e2) = eval (OrOp (LtOp e1 e2) (EqOp e1 e2))
eval (GeqOp e1 e2) = eval (OrOp (GtOp e1 e2) (EqOp e1 e2))
eval (Identifier str) = do
  mem <- getEnvs
  case lookupMem str mem of
    Just (x, _) -> return x
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (VarDef str (Type _ isMut) e) = do
  e' <- eval e
  mem <- getEnvs
  case lookup str (head mem) of
    Just _ -> fail $ "variable " ++ str ++ " is already bound."
    Nothing -> setEnvs (((str, (e', isMut)) : head mem) : tail mem) >> return Null
eval (VarAssign str e) = do
  e' <- eval e
  mem <- getEnvs
  case lookupMem str mem of
    Just (_, False) -> fail $ "variable " ++ str ++ " is not mutable."
    Just _ -> setEnvs (((str, (e', True)) : head mem) : tail mem) >> return Null
    Nothing -> fail $ "variable " ++ str ++ " is not bound."
eval (Block [] _) = do
  mem <- getEnvs
  setEnvs (tail mem)
  return Null
eval (Block (Return e : _) _) = do
  e' <- eval e
  mem <- getEnvs
  setEnvs (tail mem)
  return e'
eval (Block (e : es) isBlockStart) = do
  when isBlockStart (getEnvs >>= \mem -> setEnvs ([] : mem))
  eval e >>= \case
    (EarlyReturn v) -> do
      getEnvs >>= setEnvs . tail
      return v
    _ -> eval (Block es False)
eval (ConditionalBody []) = return Null
eval (ConditionalBody (Return e : _)) = eval e >>= \e' -> pure $ EarlyReturn e'
eval (ConditionalBody (e : es)) =
  eval e >> eval (ConditionalBody es)
eval (Return e) = do
  mem <- getEnvs
  if length mem > 1
    then eval e
    else fail "Return outside of block"
eval (If cond e1 e2) = do
  evaledCond <- eval cond
  case evaledCond of
    NumVal i | i /= 0 -> eval e1
    NumVal 0 -> case e2 of
      (Just e2') -> eval e2'
      _ -> return Null
    _ -> fail "If: type error"
eval (While cond e) = do
  evaledCond <- eval cond
  case evaledCond of
    NumVal i | i /= 0 -> eval e >> eval (While cond e)
    NumVal 0 -> return Null
    _ -> fail "While: type error"
eval (For base cond inc body) = do
  _ <- case base of
    Nothing -> return Null
    Just e -> eval e
  evaledCond <- eval cond
  case evaledCond of
    NumVal i
      | i /= 0 ->
          eval body
            >> ( case inc of
                  Nothing -> return Null
                  (Just e) -> eval e
               )
            >> eval (For Nothing cond inc body)
    NumVal 0 -> return Null
    _ -> fail "For: type error"
eval (FunctionDeclaration _type str args body) = do
  mem <- getEnvs
  case lookupMem str mem of
    Just _ -> fail $ "variable " ++ str ++ " is already bound."
    Nothing -> setEnvs (((str, (Closure args body, False)) : head mem) : tail mem) >> return Null
eval (FunctionCall id_ args) = do
  id' <- eval id_
  args' <- mapM eval args
  apply id' args'
eval (Print e) = do
  value <- eval e
  addToPrintBuffer value
  return Null
eval (ArrayAccess eArr eIndex) = do
  arr <- eval eArr
  index <- eval eIndex
  case (arr, index) of
    (ListVal xs, NumVal i) ->
      if i >= 0 && i < length xs
        then return $ xs !! i
        else fail "Array index out of bounds"
    _ -> fail "Array access: type error"
eval (EnumDeclaration _ _) = fail "Enums are not supported"
eval (StructDeclaration _ _) = fail "Structs are not supported"
eval (StructInitialization _ _) = fail "Structs are not supported"
eval (EnumElement _ _) = fail "Enums are not supported"
eval (StructElement _ _) = fail "Structs are not supported"
eval (CastToType _ _) = fail "Casting is not supported"
eval (CastToIdentifier _ _) = fail "Casting is not supported"
eval (SizeofType _) = fail "Sizeof is not supported"
eval (SizeofExpr _) = fail "Sizeof is not supported"
eval (Reference (Identifier e)) = NumVal <$> getSymbolIndex e
eval (Reference _) = fail "Reference: type error"
eval (Dereference (Identifier e)) = do
  e' <- eval $ Identifier e
  case e' of
    (NumVal i) -> getIndexValue i >>= return . fst
    _ -> fail "Dereference: type error"
eval (Dereference _) = fail "Dereference: type error"
eval (Syscall _) = fail "Syscalls are not supported"

lookupMem :: Symbol -> [Env] -> Maybe (Value, Bool)
lookupMem _ [] = Nothing
lookupMem str (env : envs) = lookup str env <|> lookupMem str envs

apply :: Value -> [Value] -> State InterpreterState Value
apply (Closure ids e) xs
  | length ids /= length xs = fail "Arguments mismatch"
  | otherwise = do
      mem <- getEnvs
      setEnvs (linkedParams : mem)
      e' <- eval e
      setEnvs mem
      return e'
 where
  linkedParams = [(sym, (val, isMut)) | ((Type _ isMut, sym), val) <- zip ids xs]
apply _ _ = fail "Expected closure"
