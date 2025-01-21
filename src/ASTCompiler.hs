{-# LANGUAGE LambdaCase #-}

module ASTCompiler (
  Instruction (..),
  compile,
  CompilerState (..),
  initialCompilerState,
  CompilerError (..),
) where

import AST
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- Define specific error types
data CompilerError
  = NotImplementedError String
  | ScopeError String
  | TypeError String
  | CompilationError String
  deriving (Show, Eq)

-- Bytecode instruction set
data Instruction
  = PushInt Int
  | PushFloat Float
  | PushNull
  | LoadVar String
  | StoreVar String
  | DefineVar String Bool
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | And
  | Or
  | Equal
  | NotEqual
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | JumpIfFalse Int
  | Jump Int
  | Call Int
  | Return
  | MakeArray Int
  | LoadArray
  | Print
  deriving (Show, Eq)

data CompilerState = CompilerState
  { instructions :: [Instruction]
  , labelCounter :: Int
  , scopeStack :: [Map.Map String Int]
  }
  deriving (Show)

initialCompilerState :: CompilerState
initialCompilerState =
  CompilerState
    { instructions = []
    , labelCounter = 0
    , scopeStack = [Map.empty]
    }

-- New type for our compiler monad stack
type Compiler a = ExceptT CompilerError (State CompilerState) a

-- Main compilation function now returns Either
compile :: Node -> Either CompilerError [Instruction]
compile node =
  let (result, _) = runState (runExceptT compileProgram) initialCompilerState
   in result
 where
  compileProgram = do
    compileNode node
    gets instructions

-- Helper to emit an instruction
emit :: Instruction -> Compiler ()
emit inst = modify $ \s -> s{instructions = instructions s ++ [inst]}

-- Helper to generate a new label
newLabel :: Compiler Int
newLabel = do
  current <- gets labelCounter
  modify $ \s -> s{labelCounter = labelCounter s + 1}
  return current

-- Compilation for each node type with proper error handling
compileNode :: Node -> Compiler ()
compileNode = \case
  IntV n -> emit $ PushInt n
  FloatV f -> emit $ PushFloat f
  ArrayV nodes -> do
    mapM_ compileNode nodes
    emit $ MakeArray (length nodes)
  Identifier sym -> emit $ LoadVar sym
  VarDef sym typ expr -> do
    compileNode expr
    emit $ DefineVar sym (mutable typ)
  VarAssign sym expr -> do
    compileNode expr
    emit $ StoreVar sym
  Block nodes isBlockStart -> do
    when isBlockStart $ modify $ \s -> s{scopeStack = Map.empty : scopeStack s}
    mapM_ compileNode nodes
    when isBlockStart $ modify $ \s -> s{scopeStack = tail $ scopeStack s}
  AST.Return expr -> do
    compileNode expr
    emit ASTCompiler.Return
  If cond thenExpr mElseExpr -> do
    endLabel <- newLabel
    elseLabel <- newLabel
    compileNode cond
    emit $ JumpIfFalse elseLabel
    compileNode thenExpr
    emit $ Jump endLabel
    emit $ JumpIfFalse elseLabel
    case mElseExpr of
      Just elseExpr -> compileNode elseExpr
      Nothing -> emit PushNull
    emit $ Jump endLabel
  While cond body -> do
    startLabel <- newLabel
    endLabel <- newLabel
    emit $ Jump startLabel
    compileNode cond
    emit $ JumpIfFalse endLabel
    compileNode body
    emit $ Jump startLabel
  FunctionCall func args -> do
    mapM_ compileNode args
    compileNode func
    emit $ Call (length args)
  AST.Print expr -> do
    compileNode expr
    emit ASTCompiler.Print
  AddOp e1 e2 -> compileBinaryOp e1 e2 Add
  SubOp e1 e2 -> compileBinaryOp e1 e2 Sub
  MulOp e1 e2 -> compileBinaryOp e1 e2 Mul
  DivOp e1 e2 -> compileBinaryOp e1 e2 Div
  ModOp e1 e2 -> compileBinaryOp e1 e2 Mod
  AndOp e1 e2 -> compileBinaryOp e1 e2 And
  OrOp e1 e2 -> compileBinaryOp e1 e2 Or
  NotOp e -> do
    compileNode e
    emit Not
  EqOp e1 e2 -> compileBinaryOp e1 e2 Equal
  NeqOp e1 e2 -> compileBinaryOp e1 e2 NotEqual
  GtOp e1 e2 -> compileBinaryOp e1 e2 Greater
  LtOp e1 e2 -> compileBinaryOp e1 e2 Less
  GeqOp e1 e2 -> compileBinaryOp e1 e2 GreaterEqual
  LeqOp e1 e2 -> compileBinaryOp e1 e2 LessEqual
  -- Unimplemented features with proper error messages
  For{} ->
    throwError $ NotImplementedError "For loops are not yet implemented"
  FunctionDeclaration{} ->
    throwError $ NotImplementedError "Function declarations are not yet implemented"
  StructDeclaration{} ->
    throwError $ NotImplementedError "Struct declarations are not yet implemented"
  EnumDeclaration{} ->
    throwError $ NotImplementedError "Enum declarations are not yet implemented"
  StructInitialization{} ->
    throwError $ NotImplementedError "Struct initialization is not yet implemented"
  EnumElement{} ->
    throwError $ NotImplementedError "Enum elements are not yet implemented"
  StructElement{} ->
    throwError $ NotImplementedError "Struct elements are not yet implemented"
  CastToType{} ->
    throwError $ NotImplementedError "Type casting is not yet implemented"
  CastToIdentifier{} ->
    throwError $ NotImplementedError "Identifier casting is not yet implemented"
  SizeofType{} ->
    throwError $ NotImplementedError "Sizeof operator is not yet implemented"
  SizeofExpr{} ->
    throwError $ NotImplementedError "Sizeof operator is not yet implemented"
  Reference{} ->
    throwError $ NotImplementedError "Reference operator is not yet implemented"
  Dereference{} ->
    throwError $ NotImplementedError "Dereference operator is not yet implemented"
  Syscall{} ->
    throwError $ NotImplementedError "Syscalls are not yet implemented"
  AddEqOp{} ->
    throwError $ NotImplementedError "Compound assignment operators are not yet implemented"
  SubEqOp{} ->
    throwError $ NotImplementedError "Compound assignment operators are not yet implemented"
  MulEqOp{} ->
    throwError $ NotImplementedError "Compound assignment operators are not yet implemented"
  DivEqOp{} ->
    throwError $ NotImplementedError "Compound assignment operators are not yet implemented"
  _ -> throwError $ CompilationError "Unknown node type"

-- Helper for compiling binary operations
compileBinaryOp :: Node -> Node -> Instruction -> Compiler ()
compileBinaryOp e1 e2 op = do
  compileNode e1
  compileNode e2
  emit op
