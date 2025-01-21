{-# LANGUAGE LambdaCase #-}

module ASTCompiler (
  Instruction (..),
  compile,
  CompilerState (..),
  initialCompilerState,
  CompilerError (..),
  Scope (..),
) where

import AST
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

data CompilerError
  = NotImplementedError String
  | ScopeError String
  | TypeError String
  | CompilationError String
  deriving (Show, Eq)

-- Enhanced instruction set with stack frame management
data Instruction
  = PushInt Int
  | PushFloat Float
  | PushNull
  | LoadVar String
  | StoreVar String
  | DefineVar String Bool
  | Pop
  | -- Stack frame management instructions
    PushFrame -- Create new stack frame
  | PopFrame -- Remove current stack frame
  | StoreLocal String -- Store in current frame
  | LoadLocal String -- Load from current frame
  | StoreGlobal String -- Store in global scope
  | LoadGlobal String -- Load from global scope
  -- Rest of instructions
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

-- Enhanced scope tracking
data Scope = Scope
  { variables :: Map.Map String (Int, Bool) -- (stack offset, is mutable)
  , scopeDepth :: Int
  }
  deriving (Show)

data CompilerState = CompilerState
  { instructions :: [Instruction]
  , labelCounter :: Int
  , scopeStack :: [Scope] -- Stack of scopes
  , currentStackOffset :: Int -- Current stack position
  }
  deriving (Show)

initialCompilerState :: CompilerState
initialCompilerState =
  CompilerState
    { instructions = []
    , labelCounter = 0
    , scopeStack = [Scope Map.empty 0] -- Global scope
    , currentStackOffset = 0
    }

type Compiler a = ExceptT CompilerError (State CompilerState) a

-- Helper functions for scope management
pushScope :: Compiler ()
pushScope = do
  state' <- lift get
  let depth = case scopeStack state' of
        [] -> 0
        (s : _) -> scopeDepth s + 1
  let newScope = Scope Map.empty depth
  modify $ \s -> s{scopeStack = newScope : scopeStack s}
  emit PushFrame

popScope :: Compiler ()
popScope = do
  state' <- lift get
  case scopeStack state' of
    [] -> throwError $ ScopeError "No active scope"
    [_globalScope] -> throwError $ ScopeError "Cannot pop global scope"
    _globalScope : rest -> do
      modify $ \s -> s{scopeStack = rest}
      emit PopFrame

defineVariable :: String -> Bool -> Compiler ()
defineVariable name isMutable = do
  state' <- lift get
  case scopeStack state' of
    [] -> throwError $ ScopeError "No active scope"
    (scope : rest) ->
      if Map.member name (variables scope)
        then
          throwError $
            ScopeError $
              "Variable " ++ name ++ " already defined in current scope"
        else
          let offset = currentStackOffset state'
              newScope = scope{variables = Map.insert name (offset, isMutable) (variables scope)}
           in modify $ \s -> s{scopeStack = newScope : rest, currentStackOffset = offset + 1}

lookupVariable :: String -> Compiler (Bool, Bool) -- (isGlobal, isMutable)
lookupVariable name = do
  state' <- lift get
  case findVariable name (scopeStack state') of
    Nothing -> throwError $ ScopeError $ "Variable " ++ name ++ " not found"
    Just (isGlobal, isMutable) -> return (isGlobal, isMutable)
 where
  findVariable :: String -> [Scope] -> Maybe (Bool, Bool)
  findVariable _ [] = Nothing
  findVariable str (s : ss) =
    case Map.lookup str (variables s) of
      Just (_, mut) -> Just (scopeDepth s == 0, mut)
      Nothing -> findVariable str ss

-- Helper functions
emit :: Instruction -> Compiler ()
emit inst = modify $ \s -> s{instructions = instructions s ++ [inst]}

newLabel :: Compiler Int
newLabel = do
  current <- gets labelCounter
  modify $ \s -> s{labelCounter = labelCounter s + 1}
  return current

-- Main compilation function
compile :: Node -> Either CompilerError [Instruction]
compile node =
  let (result, _) = runState (runExceptT compileProgram) initialCompilerState
   in result
 where
  compileProgram = do
    compileNode node
    gets instructions

-- Node compilation with scope awareness
compileNode :: Node -> Compiler ()
compileNode = \case
  IntV n -> emit $ PushInt n
  FloatV f -> emit $ PushFloat f
  ArrayV nodes -> do
    mapM_ compileNode nodes
    emit $ MakeArray (length nodes)
  Identifier sym -> do
    (isGlobal, _) <- lookupVariable sym
    emit $
      if isGlobal
        then LoadGlobal sym
        else LoadLocal sym
  VarDef sym typ expr -> do
    compileNode expr
    defineVariable sym (mutable typ)
    emit $ DefineVar sym (mutable typ)
  VarAssign sym expr -> do
    (isGlobal, isMutable) <- lookupVariable sym
    unless isMutable $
      throwError $
        ScopeError $
          "Cannot assign to immutable variable " ++ sym
    compileNode expr
    emit $
      if isGlobal
        then StoreGlobal sym
        else StoreLocal sym
  Block nodes isBlockStart -> do
    when isBlockStart pushScope
    mapM_ compileNode nodes
    when isBlockStart popScope
  AST.Return expr -> do
    compileNode expr
    emit ASTCompiler.Return
  If cond thenExpr maybeElseExpr -> do
    endLabel <- newLabel
    elseLabel <- newLabel
    compileNode cond
    emit $ JumpIfFalse elseLabel
    compileNode thenExpr
    emit $ Jump endLabel
    emit $ JumpIfFalse elseLabel
    case maybeElseExpr of
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
  NotOp e -> compileNode e >> emit Not
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
  FunctionCall{} ->
    throwError $ NotImplementedError "Function calls are not yet implemented"
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
compileBinaryOp e1 e2 op = compileNode e1 >> compileNode e2 >> emit op
