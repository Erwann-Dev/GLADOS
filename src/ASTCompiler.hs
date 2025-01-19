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

eval :: Node -> Bytecode
eval (IntV nb) = [Test]
eval (FloatV nb) = [Test]
eval (ArrayV nodes) = [Test]
eval (VarDef dest type_ value) = [Test]
eval (VarAssign dest value) = [Test]
eval (Block nodes) = [Test]
eval (Return node) = [Test]
eval (If condition then_ else_) = [Test]
eval (While condition body) = [Test]
eval (For init condition inc body) = [Test]
eval (FunctionDeclaration type_ name args body) = [Test]
eval (FunctionCall name args) = [Test]
eval (EnumDeclaration name elements) = [Test]
eval (StructDeclaration name elements) = [Test]
eval (StructInitialization name elements) = [Test]
eval (EnumElement name value) = [Test]
eval (StructElement name value) = [Test]
eval (CastToType type_ value) = [Test]
eval (CastToIdentifier type_ value) = [Test]
eval (LtOp left right) = [Test]
eval (GtOp left right) = [Test]
eval (LeqOp left right) = [Test]
eval (GeqOp left right) = [Test]
eval (EqOp left right) = [Test]
eval (NeqOp left right) = [Test]
eval (AndOp left right) = [Test]
eval (OrOp left right) = [Test]
eval (NotOp value) = [Test]
eval (AddOp left right) = [Test]
eval (AddEqOp left right) = [Test]
eval (SubOp left right) = [Test]
eval (SubEqOp left right) = [Test]
eval (MulOp left right) = [Test]
eval (MulEqOp left right) = [Test]
eval (DivOp left right) = [Test]
eval (DivEqOp left right) = [Test]
eval (ModOp left right) = [Test]
eval (SizeofExpr expr) = [Test]
eval (SizeofType type_) = [Test]
eval (Reference value) = [Test]
eval (Dereference value) = [Test]
eval (ArrayAccess array index) = [Test]
eval (Syscall args) = [Test]
eval (Identifier name) = [Test]
