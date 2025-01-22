module ASTVM (
  VMValue (..),
  VMState (..),
  executeVM,
  runVM,
  loadBytecode,
  initialVMState,
  wordToFloat,
) where

import Bytecode (OpCode (..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.Map as Map
import Data.Word

-- | Values that can be stored in the VM
data VMValue
  = VMInt Int32
  | VMFloat Float
  | VMNull
  | VMArray [VMValue]
  deriving (Eq)

instance Show VMValue where
  show (VMInt x) = show x
  show (VMFloat x) = show x
  show VMNull = "null"
  show (VMArray xs) = "[" ++ unwords (map show xs) ++ "]"

-- | Frame for storing local variables
data Frame = Frame
  { locals :: Map.Map String VMValue
  , returnAddress :: Int
  }
  deriving (Show)

-- | VM State
data VMState = VMState
  { stack :: [VMValue]
  , frames :: [Frame]
  , globals :: Map.Map String VMValue
  , ip :: Int -- Instruction pointer
  , bytecode :: BS.ByteString
  , output :: [String] -- Output buffer for print instructions
  }
  deriving (Show)

-- | VM monad
type VM a = ExceptT String (State VMState) a

-- | Initial VM state
initialVMState :: VMState
initialVMState =
  VMState
    { stack = []
    , frames = []
    , globals = Map.empty
    , ip = 0
    , bytecode = BS.empty
    , output = []
    }

-- | Load bytecode into VM state
loadBytecode :: BS.ByteString -> VMState -> VMState
loadBytecode code state' = state'{bytecode = code, ip = 0}

-- | Read bytes from current instruction pointer
readBytes :: Int -> VM BS.ByteString
readBytes n = do
  state' <- get
  let currentIp = ip state'
  let code = bytecode state'
  if currentIp + n <= BS.length code
    then do
      put state'{ip = currentIp + n}
      return $ BS.take n $ BS.drop currentIp code
    else throwError "Unexpected end of bytecode"

-- | Read a string from bytecode (length prefixed)
readString :: VM String
readString = do
  lenBytes <- readBytes 2
  let len = fromIntegral $ runGet getWord16be $ LBS.fromStrict lenBytes
  strBytes <- readBytes len
  return $ map (toEnum . fromIntegral) $ BS.unpack strBytes

-- | Stack operations
push :: VMValue -> VM ()
push val = modify $ \s -> s{stack = val : stack s}

pop :: VM VMValue
pop = do
  state' <- get
  case stack state' of
    [] -> throwError "Stack underflow"
    (x : xs) -> do
      put state'{stack = xs}
      return x

-- | Frame operations
pushFrame :: VM ()
pushFrame = modify $ \s -> s{frames = Frame Map.empty (ip s) : frames s}

popFrame :: VM ()
popFrame = do
  state' <- get
  case frames state' of
    [] -> throwError "No frame to pop"
    (_ : fs) -> put state'{frames = fs}

-- | Variable operations
storeLocal :: String -> VMValue -> VM ()
storeLocal name val = do
  state' <- get
  case frames state' of
    [] -> throwError "No active frame"
    (frame : rest) -> do
      let newFrame = frame{locals = Map.insert name val (locals frame)}
      put state'{frames = newFrame : rest}

loadLocal :: String -> VM VMValue
loadLocal name = do
  state' <- get
  case frames state' of
    [] -> throwError "No active frame"
    (frame : _) -> case Map.lookup name (locals frame) of
      Nothing -> throwError $ "Local variable not found: " ++ name
      Just val -> return val

storeGlobal :: String -> VMValue -> VM ()
storeGlobal name val = modify $ \s -> s{globals = Map.insert name val (globals s)}

loadGlobal :: String -> VM VMValue
loadGlobal name = do
  state' <- get
  case Map.lookup name (globals state') of
    Nothing -> throwError $ "Global variable not found: " ++ name
    Just val -> return val

-- | Binary operations
binaryOp :: (VMValue -> VMValue -> VM VMValue) -> VM ()
binaryOp op = do
  b <- pop
  a <- pop
  result <- op a b
  push result

numericOp :: (Int32 -> Int32 -> Int32) -> (Float -> Float -> Float) -> VMValue -> VMValue -> VM VMValue
numericOp intOp floatOp a b = case (a, b) of
  (VMInt x, VMInt y) -> return $ VMInt (intOp x y)
  (VMFloat x, VMFloat y) -> return $ VMFloat (floatOp x y)
  (VMInt x, VMFloat y) -> return $ VMFloat (floatOp (fromIntegral x) y)
  (VMFloat x, VMInt y) -> return $ VMFloat (floatOp x (fromIntegral y))
  _ -> throwError "Type mismatch in numeric operation"

-- | Execute a single instruction
executeInstruction :: Word8 -> VM ()
executeInstruction opcode = case toEnum (fromIntegral opcode) of
  OP_PUSH_INT -> do
    bytes <- readBytes 4
    let val = runGet getInt32be $ LBS.fromStrict bytes
    push $ VMInt val
  OP_PUSH_FLOAT -> do
    bytes <- readBytes 4
    let bits = runGet getWord32be $ LBS.fromStrict bytes
    push $ VMFloat $ wordToFloat bits
  OP_PUSH_NULL -> push VMNull
  OP_LOAD_VAR -> do
    name <- readString
    val <- loadLocal name
    push val
  OP_STORE_VAR -> do
    name <- readString
    val <- pop
    storeLocal name val
  OP_DEFINE_VAR -> do
    name <- readString
    bytes <- readBytes 1
    let _ = BS.head bytes /= 0
    val <- pop
    storeLocal name val
  OP_POP -> void pop
  OP_PUSH_FRAME -> pushFrame
  OP_POP_FRAME -> popFrame
  OP_STORE_LOCAL -> do
    name <- readString
    val <- pop
    storeLocal name val
  OP_LOAD_LOCAL -> do
    name <- readString
    val <- loadLocal name
    push val
  OP_STORE_GLOBAL -> do
    name <- readString
    val <- pop
    storeGlobal name val
  OP_LOAD_GLOBAL -> do
    name <- readString
    val <- loadGlobal name
    push val
  OP_ADD -> binaryOp $ numericOp (+) (+)
  OP_SUB -> binaryOp $ numericOp (-) (-)
  OP_MUL -> binaryOp $ numericOp (*) (*)
  OP_DIV -> binaryOp $ numericOp div (/)
  OP_MOD -> binaryOp $ \a b -> case (a, b) of
    (VMInt x, VMInt y) | y /= 0 -> return $ VMInt (x `mod` y)
    _ -> throwError "Invalid modulo operation"
  OP_NOT -> do
    val <- pop
    case val of
      VMInt x -> push $ VMInt $ if x == 0 then 1 else 0
      _ -> throwError "NOT operation requires integer"
  OP_EQUAL -> do
    b <- pop
    a <- pop
    push $ VMInt $ if a == b then 1 else 0
  OP_NOT_EQUAL -> do
    b <- pop
    a <- pop
    push $ VMInt $ if a /= b then 1 else 0
  OP_GREATER -> binaryOp $ numericOp (\x y -> if x > y then 1 else 0) (\x y -> if x > y then 1 else 0)
  OP_LESS -> binaryOp $ numericOp (\x y -> if x < y then 1 else 0) (\x y -> if x < y then 1 else 0)
  OP_GREATER_EQUAL -> binaryOp $ numericOp (\x y -> if x >= y then 1 else 0) (\x y -> if x >= y then 1 else 0)
  OP_LESS_EQUAL -> binaryOp $ numericOp (\x y -> if x <= y then 1 else 0) (\x y -> if x <= y then 1 else 0)
  OP_AND -> do
    b <- pop
    a <- pop
    push $ VMInt $ if a == VMInt 1 && b == VMInt 1 then 1 else 0
  OP_OR -> do
    b <- pop
    a <- pop
    push $ VMInt $ if a == VMInt 1 || b == VMInt 1 then 1 else 0
  OP_PRINT -> do
    val <- pop
    modify $ \s -> s{output = show val : output s}
  OP_MAKE_ARRAY -> do
    bytes <- readBytes 4
    let size = fromIntegral $ runGet getInt32be $ LBS.fromStrict bytes
    elements <- replicateM size pop
    push $ VMArray $ reverse elements
  OP_JUMP -> do
    bytes <- readBytes 4
    let offset = fromIntegral $ runGet getInt32be $ LBS.fromStrict bytes
    modify $ \s -> s{ip = offset}
  OP_JUMP_IF_FALSE -> do
    bytes <- readBytes 4
    let offset = fromIntegral $ runGet getInt32be $ LBS.fromStrict bytes
    val <- pop
    case val of
      VMInt 0 -> modify $ \s -> s{ip = offset}
      VMInt _ -> return ()
      _ -> throwError "JUMP_IF_FALSE requires integer condition"
  _ -> throwError $ "Unknown opcode: " ++ show opcode

-- | Helper function to convert Word32 to Float
wordToFloat :: Word32 -> Float
wordToFloat w = decodeFloat32 $ fromIntegral w
 where
  decodeFloat32 :: Int32 -> Float
  decodeFloat32 bits =
    let sign = if bits < 0 then -1 else 1
        exp' = fromIntegral ((bits `shiftR` 23) .&. 0xFF) - 127
        mantissa = fromIntegral (bits .&. 0x7FFFFF) / (2 ^ (23 :: Integer))
     in sign * (1 + mantissa) * (2 ** exp')

-- | Main execution loop
executeVM :: VM ()
executeVM = do
  state' <- get
  if ip state' >= BS.length (bytecode state')
    then return ()
    else do
      bytes <- readBytes 1
      executeInstruction (BS.head bytes)
      executeVM

-- | Run the VM with given bytecode
runVM :: BS.ByteString -> Either String VMState
runVM code =
  let initialState = loadBytecode code initialVMState
      (result, finalState) = runState (runExceptT executeVM) initialState
   in case result of
        Left err -> Left err
        Right _ -> Right finalState
