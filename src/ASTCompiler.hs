{-# LANGUAGE LambdaCase #-}

module ASTCompiler (
  Instruction (..),
  compile,
  CompilerState (..),
  initialCompilerState,
) where

import AST
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

-- Bytecode instruction set
data Instruction
  = PushInt Int -- Push integer constant
  | PushFloat Float -- Push float constant
  | PushNull -- Push null value
  | LoadVar String -- Load variable from environment
  | StoreVar String -- Store value in variable
  | DefineVar String Bool -- Define new variable (with mutability flag)
  | Pop -- Pop value from stack
  | Add -- Add top two values
  | Sub -- Subtract
  | Mul -- Multiply
  | Div -- Divide
  | Mod -- Modulo
  | Not -- Logical not
  | And -- Logical and
  | Or -- Logical or
  | Equal -- Equality comparison
  | NotEqual -- Inequality comparison
  | Greater -- Greater than
  | Less -- Less than
  | GreaterEqual -- Greater than or equal
  | LessEqual -- Less than or equal
  | JumpIfFalse Int -- Conditional jump
  | Jump Int -- Unconditional jump
  | Call Int -- Function call with arg count
  | Return -- Return from function
  | MakeArray Int -- Create array with n elements
  | LoadArray -- Load array element
  | Print -- Print value
  deriving (Show, Eq)

-- Compiler state
data CompilerState = CompilerState
  { instructions :: [Instruction] -- Generated instructions
  , labelCounter :: Int -- Counter for generating unique labels
  , scopeStack :: [Map.Map String Int] -- Stack of variable scopes
  }
  deriving (Show)

initialCompilerState :: CompilerState
initialCompilerState =
  CompilerState
    { instructions = []
    , labelCounter = 0
    , scopeStack = [Map.empty]
    }

type Compiler a = State CompilerState a

-- Main compilation function
compile :: Node -> ([Instruction], CompilerState)
compile node = runState instructionList initialCompilerState
 where
  instructionList = do
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

-- Compilation for each node type
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
  _ -> error "Compilation not implemented"

-- Helper for compiling binary operations
compileBinaryOp :: Node -> Node -> Instruction -> Compiler ()
compileBinaryOp e1 e2 op = do
  compileNode e1
  compileNode e2
  emit op
