module ASTCompiler (

)
where

import AST (Node (..))
import Bytecode (Bytecode, Instruction (..))

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

setState :: a -> State a ()
setState newState = State $ \_ -> Right ((), newState)

getState :: State a a
getState = State $ \state -> Right (state, state)

eval :: Node -> State [Int] Bytecode
eval (IntV nb) = pure [Test]
eval (FloatV nb) = pure [Test]
eval (ArrayV nodes) = pure [Test]
eval (VarDef dest type_ value) = pure [Test]
eval (VarAssign dest value) = pure [Test]
eval (Block nodes) = pure [Test]
eval (Return node) = pure [Test]
eval (If condition then_ else_) = pure [Test]
eval (While condition body) = pure [Test]
eval (For init condition inc body) = pure [Test]
eval (FunctionDeclaration type_ name args body) = pure [Test]
eval (FunctionCall name args) = pure [Test]
eval (EnumDeclaration name elements) = pure [Test]
eval (StructDeclaration name elements) = pure [Test]
eval (StructInitialization name elements) = pure [Test]
eval (EnumElement name value) = pure [Test]
eval (StructElement name value) = pure [Test]
eval (CastToType type_ value) = pure [Test]
eval (CastToIdentifier type_ value) = pure [Test]
eval (LtOp left right) = pure [Test]
eval (GtOp left right) = pure [Test]
eval (LeqOp left right) = pure [Test]
eval (GeqOp left right) = pure [Test]
eval (EqOp left right) = pure [Test]
eval (NeqOp left right) = pure [Test]
eval (AndOp left right) = pure [Test]
eval (OrOp left right) = pure [Test]
eval (NotOp value) = pure [Test]
eval (AddOp left right) = pure [Test]
eval (AddEqOp left right) = pure [Test]
eval (SubOp left right) = pure [Test]
eval (SubEqOp left right) = pure [Test]
eval (MulOp left right) = pure [Test]
eval (MulEqOp left right) = pure [Test]
eval (DivOp left right) = pure [Test]
eval (DivEqOp left right) = pure [Test]
eval (ModOp left right) = pure [Test]
eval (SizeofExpr expr) = pure [Test]
eval (SizeofType type_) = pure [Test]
eval (Reference value) = pure [Test]
eval (Dereference value) = pure [Test]
eval (ArrayAccess array index) = pure [Test]
eval (Syscall args) = pure [Test]
eval (Identifier name) = pure [Test]
