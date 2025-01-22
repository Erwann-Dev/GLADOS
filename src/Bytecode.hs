module Bytecode (
  Bytecode,
  Instruction,
  toBytecode,
  OpCode (..),
  writeBytecodeToFile,
  floatToBytes,
) where

import CompilerInstruction
import Control.Exception (SomeException, try)
import Data.Bits hiding (And)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import System.Directory
import System.FilePath
import System.IO

-- | Type alias for our bytecode
type Bytecode = BS.ByteString

-- | OpCodes for our bytecode instructions
data OpCode
  = OP_PUSH_INT -- 0x00
  | OP_PUSH_FLOAT -- 0x01
  | OP_PUSH_NULL -- 0x02
  | OP_LOAD_VAR -- 0x03
  | OP_STORE_VAR -- 0x04
  | OP_DEFINE_VAR -- 0x05
  | OP_POP -- 0x06
  | OP_PUSH_FRAME -- 0x07
  | OP_POP_FRAME -- 0x08
  | OP_STORE_LOCAL -- 0x09
  | OP_LOAD_LOCAL -- 0x0A
  | OP_STORE_GLOBAL -- 0x0B
  | OP_LOAD_GLOBAL -- 0x0C
  | OP_ADD -- 0x0D
  | OP_SUB -- 0x0E
  | OP_MUL -- 0x0F
  | OP_DIV -- 0x10
  | OP_MOD -- 0x11
  | OP_NOT -- 0x12
  | OP_AND -- 0x13
  | OP_OR -- 0x14
  | OP_EQUAL -- 0x15
  | OP_NOT_EQUAL -- 0x16
  | OP_GREATER -- 0x17
  | OP_LESS -- 0x18
  | OP_GREATER_EQUAL -- 0x19
  | OP_LESS_EQUAL -- 0x1A
  | OP_JUMP_IF_FALSE -- 0x1B
  | OP_JUMP -- 0x1C
  | OP_CALL -- 0x1D
  | OP_RETURN -- 0x1E
  | OP_MAKE_ARRAY -- 0x1F
  | OP_LOAD_ARRAY -- 0x20
  | OP_PRINT -- 0x21
  deriving (Enum, Show)

-- | Convert OpCode to Word8
opCodeToBytes :: OpCode -> Builder
opCodeToBytes = word8 . fromIntegral . fromEnum

-- | Convert a string to bytes with length prefix
stringToBytes :: String -> Builder
stringToBytes str = word16BE (fromIntegral $ length str) <> foldMap (word8 . fromIntegral . fromEnum) str

-- | Convert an Int32 to bytes
int32ToBytes :: Int -> Builder
int32ToBytes n = int32BE (fromIntegral n)

-- | Convert a Float to bytes
floatToBytes :: Float -> Builder
floatToBytes f = int32BE (fromIntegral (floatToWord32 f))
 where
  floatToWord32 :: Float -> Word32
  floatToWord32 x = loWord .|. (hiWord `shiftL` 16)
   where
    bits = floor (x * (2 ^ (32 :: Integer))) :: Integer
    loWord = fromIntegral (bits .&. 0xFFFF) :: Word32
    hiWord = fromIntegral ((bits `shiftR` 16) .&. 0xFFFF) :: Word32

-- | Convert a boolean to a byte
boolToByte :: Bool -> Builder
boolToByte b = word8 (if b then 1 else 0)

-- | Convert a single instruction to bytecode
instructionToBytes :: Instruction -> Builder
instructionToBytes instr = case instr of
  PushInt n -> opCodeToBytes OP_PUSH_INT <> int32ToBytes n
  PushFloat f -> opCodeToBytes OP_PUSH_FLOAT <> floatToBytes f
  PushNull -> opCodeToBytes OP_PUSH_NULL
  LoadVar s -> opCodeToBytes OP_LOAD_VAR <> stringToBytes s
  StoreVar s -> opCodeToBytes OP_STORE_VAR <> stringToBytes s
  DefineVar s mut -> opCodeToBytes OP_DEFINE_VAR <> stringToBytes s <> boolToByte mut
  Pop -> opCodeToBytes OP_POP
  PushFrame -> opCodeToBytes OP_PUSH_FRAME
  PopFrame -> opCodeToBytes OP_POP_FRAME
  StoreLocal s -> opCodeToBytes OP_STORE_LOCAL <> stringToBytes s
  LoadLocal s -> opCodeToBytes OP_LOAD_LOCAL <> stringToBytes s
  StoreGlobal s -> opCodeToBytes OP_STORE_GLOBAL <> stringToBytes s
  LoadGlobal s -> opCodeToBytes OP_LOAD_GLOBAL <> stringToBytes s
  Add -> opCodeToBytes OP_ADD
  Sub -> opCodeToBytes OP_SUB
  Mul -> opCodeToBytes OP_MUL
  Div -> opCodeToBytes OP_DIV
  Mod -> opCodeToBytes OP_MOD
  Not -> opCodeToBytes OP_NOT
  And -> opCodeToBytes OP_AND
  Or -> opCodeToBytes OP_OR
  Equal -> opCodeToBytes OP_EQUAL
  NotEqual -> opCodeToBytes OP_NOT_EQUAL
  Greater -> opCodeToBytes OP_GREATER
  Less -> opCodeToBytes OP_LESS
  GreaterEqual -> opCodeToBytes OP_GREATER_EQUAL
  LessEqual -> opCodeToBytes OP_LESS_EQUAL
  JumpIfFalse offset -> opCodeToBytes OP_JUMP_IF_FALSE <> int32ToBytes offset
  Jump offset -> opCodeToBytes OP_JUMP <> int32ToBytes offset
  Call n -> opCodeToBytes OP_CALL <> int32ToBytes n
  Return -> opCodeToBytes OP_RETURN
  MakeArray size -> opCodeToBytes OP_MAKE_ARRAY <> int32ToBytes size
  LoadArray -> opCodeToBytes OP_LOAD_ARRAY
  Print -> opCodeToBytes OP_PRINT

-- | Convert a list of instructions to bytecode
toBytecode :: [Instruction] -> Bytecode
toBytecode instructions =
  BS.concat $ LBS.toChunks $ toLazyByteString $ mconcat $ map instructionToBytes instructions

writeBytecodeToFile :: FilePath -> Bytecode -> IO (Either String String)
writeBytecodeToFile filepath bytecode = do
  -- Create directory if it doesn't exist
  let dir = takeDirectory filepath
  createDirectoryIfMissing True dir

  -- Check if file already exists
  fileExists <- doesFileExist filepath
  if fileExists
    then return $ Left $ "Error: File " ++ filepath ++ " already exists"
    else do
      -- Try to write the bytecode to the file
      result <- try $ do
        handle <- openFile filepath WriteMode
        BS.hPut handle bytecode
        hClose handle
      case result of
        Left e -> return $ Left $ "Error writing to file: " ++ show (e :: SomeException)
        Right _ -> return $ Right $ "Successfully wrote bytecode to " ++ filepath
