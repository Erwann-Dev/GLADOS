{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module ASTCompiler (
  Instruction (..),
  compile,
  CompilerState (..),
  initialCompilerState,
  CompilerError (..),
  Scope (..),
) where

import AST
import CompilerInstruction as Instr
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

-- Enhanced scope tracking
data Scope = Scope
  { variables :: Map.Map String (Int, Type) -- (stack offset, is mutable, type)
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
    , scopeStack = [] -- Global scope
    , currentStackOffset = 0
    }

type Compiler a = ExceptT CompilerError (State CompilerState) a

checkTypes :: BasicType -> BasicType -> Compiler ()
checkTypes INT INT = return ()
checkTypes FLOAT FLOAT = return ()
checkTypes t1 t2 = throwError $ TypeError $ "Type mismatch: expected " ++ show t1 ++ " but got " ++ show t2

checkMutability :: String -> Bool -> Compiler ()
checkMutability sym isMutable =
  unless isMutable $
    throwError $
      ScopeError $
        "Cannot assign to immutable variable " ++ sym

checkIfFloat :: BasicType -> Compiler ()
checkIfFloat varType =
  unless (isIntegerType varType) $
    throwError $
      TypeError "Type mismatch: expected float but got integer"

checkIfInteger :: BasicType -> Compiler ()
checkIfInteger varType =
  when (isIntegerType varType) $
    throwError $
      TypeError "Type mismatch: expected integer but got float"

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

-- Modified define variable to use BasicType
defineVariable :: String -> Type -> Compiler ()
defineVariable name typ = do
  state' <- get
  case scopeStack state' of
    [] -> throwError $ ScopeError "No active scope"
    (scope : rest) -> do
      when (Map.member name (variables scope)) $
        throwError $
          ScopeError $
            "Variable " ++ name ++ " already defined in current scope"
      let offset = currentStackOffset state'
      let newScope = scope{variables = Map.insert name (offset, typ) (variables scope)}
      modify $ \s ->
        s
          { scopeStack = newScope : rest
          , currentStackOffset = offset + 1
          }

-- Modified lookup to work with BasicType
lookupVariable :: String -> Compiler (Bool, Type) -- (isGlobal, isMutable, basic_type)
lookupVariable name = do
  state' <- get
  case findVariable name (scopeStack state') of
    Nothing -> throwError $ ScopeError $ "Variable " ++ name ++ " not found"
    Just (isGlobal, typ) -> return (isGlobal, typ)
 where
  findVariable :: String -> [Scope] -> Maybe (Bool, Type)
  findVariable _ [] = Nothing
  findVariable n (s : ss) =
    case Map.lookup n (variables s) of
      Just (_, typ) -> Just (scopeDepth s == 0, typ)
      Nothing -> findVariable n ss

-- Helper to get type of a node
getNodeType :: Node -> Compiler BasicType
getNodeType (IntV _) = return INT
getNodeType (FloatV _) = return FLOAT
getNodeType (ArrayV nodes) =
  if null nodes
    then return Void
    else getNodeType (head nodes) >>= \typ -> checkArrType typ >> return typ
 where
  checkArrType typ = mapM_ (f typ) nodes
  f typ node = do
    t <- getNodeType node
    when (t /= typ) $ throwError $ TypeError "Array elements must have the same type"
getNodeType (Identifier name) = lookupVariable name >>= \(_, Type typ _) -> return typ
getNodeType (AddOp e1 e2) = getCommonNumericType e1 e2
getNodeType (SubOp e1 e2) = getCommonNumericType e1 e2
getNodeType (MulOp e1 e2) = getCommonNumericType e1 e2
getNodeType (DivOp e1 e2) = getCommonNumericType e1 e2
getNodeType (ModOp _ _) = return INT
getNodeType (NotOp _) = return INT
getNodeType (EqOp _ _) = return INT
getNodeType (NeqOp _ _) = return INT
getNodeType (GtOp _ _) = return INT
getNodeType (LtOp _ _) = return INT
getNodeType (GeqOp _ _) = return INT
getNodeType (LeqOp _ _) = return INT
getNodeType _ = throwError $ TypeError "Cannot determine type of expression"

-- Helper for numeric operations
getCommonNumericType :: Node -> Node -> Compiler BasicType
getCommonNumericType e1 e2 = do
  t1 <- getNodeType e1
  t2 <- getNodeType e2
  checkTypes t1 t2
  return t1

isIntegerType :: BasicType -> Bool
isIntegerType t = t == INT

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
    emit $ if isGlobal then LoadGlobal sym else LoadLocal sym
  VarDef sym typ (IntV n) -> do
    checkIfFloat $ basic_type typ
    compileNode (IntV n)
    defineVariable sym typ
    emit $ DefineVar sym (mutable typ)
  VarDef sym typ (FloatV n) -> do
    checkIfInteger $ basic_type typ
    compileNode (FloatV n)
    defineVariable sym typ
    emit $ DefineVar sym (mutable typ)
  VarDef sym typ expr -> do
    exprType <- getNodeType expr
    checkTypes (basic_type typ) exprType
    compileNode expr
    defineVariable sym typ
    emit $ DefineVar sym (mutable typ)
  VarAssign sym (IntV n) -> do
    (isGlobal, Type varType isMutable) <- lookupVariable sym
    checkMutability sym isMutable
    checkIfFloat varType
    compileNode (IntV n)
    emit $ if isGlobal then StoreGlobal sym else StoreLocal sym
  VarAssign sym (FloatV n) -> do
    (isGlobal, Type varType isMutable) <- lookupVariable sym
    checkMutability sym isMutable
    checkIfInteger varType
    compileNode (FloatV n)
    emit $ if isGlobal then StoreGlobal sym else StoreLocal sym
  VarAssign sym expr -> do
    (isGlobal, Type varType isMutable) <- lookupVariable sym
    checkMutability sym isMutable
    exprType <- getNodeType expr
    checkTypes varType exprType
    compileNode expr
    emit $ if isGlobal then StoreGlobal sym else StoreLocal sym
  Block nodes isBlockStart -> do
    when isBlockStart pushScope
    mapM_ compileNode nodes
    when isBlockStart popScope
  AST.Return expr -> do
    compileNode expr
    emit Instr.Return
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
    emit Instr.Print
  AddOp e1 e2 -> compileBinaryOp e1 e2 Add
  SubOp e1 e2 -> compileBinaryOp e1 e2 Sub
  MulOp e1 e2 -> compileBinaryOp e1 e2 Mul
  DivOp e1 e2 -> compileBinaryOp e1 e2 Div
  ModOp e1 e2 -> compileBinaryOp e1 e2 Mod
  AndOp e1 e2 -> compileBinaryOp e1 e2 And
  AddEqOp sym e2 -> compileNode (VarAssign sym (AddOp (Identifier sym) e2))
  SubEqOp sym e2 -> compileNode (VarAssign sym (SubOp (Identifier sym) e2))
  MulEqOp sym e2 -> compileNode (VarAssign sym (MulOp (Identifier sym) e2))
  DivEqOp sym e2 -> compileNode (VarAssign sym (DivOp (Identifier sym) e2))
  OrOp e1 e2 -> compileBinaryOp e1 e2 Or
  NotOp e -> compileNode e >> emit Not
  EqOp e1 e2 -> compileBinaryOp e1 e2 Equal
  NeqOp e1 e2 -> compileBinaryOp e1 e2 NotEqual
  GtOp e1 e2 -> compileBinaryOp e1 e2 Greater
  LtOp e1 e2 -> compileBinaryOp e1 e2 Less
  GeqOp e1 e2 -> compileBinaryOp e1 e2 GreaterEqual
  LeqOp e1 e2 -> compileBinaryOp e1 e2 LessEqual
  ConditionalBody nodes -> mapM_ compileNode nodes
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
  _ -> throwError $ CompilationError "Unknown node type"

-- Helper for compiling binary operations
compileBinaryOp :: Node -> Node -> Instruction -> Compiler ()
compileBinaryOp e1 e2 op = do
  t1 <- getNodeType e1
  t2 <- getNodeType e2
  checkTypes t1 t2
  compileNode e1
  compileNode e2
  emit op
