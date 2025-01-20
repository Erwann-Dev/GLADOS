module AST (
  Node (..),
  Type (..),
  BasicType (..),
  TypeSize,
  Symbol,
) where

type TypeSize = Int

type Symbol = String

data Type = Type
  { basic_type :: BasicType
  , mutable :: Bool
  }
  deriving (Show, Eq)

data BasicType
  = U8
  | U16
  | U32
  | I8
  | I16
  | I32
  | F32
  | Void
  | Pointer Type
  deriving (Show, Eq)

data Node
  = IntV Int
  | FloatV Float
  | ArrayV [Node]
  | Identifier Symbol
  | VarDef Symbol Type Node
  | VarAssign Symbol Node
  | Block [Node] Bool
  | Return Node
  | If Node Node (Maybe Node)
  | While Node Node
  | For (Maybe Node) Node (Maybe Node) Node
  | FunctionDeclaration Type Symbol [(Type, Symbol)] Node
  | FunctionCall Node [Node]
  | EnumDeclaration Symbol [Node]
  | StructDeclaration Symbol [(Type, Symbol)]
  | StructInitialization Node [(Node, Node)]
  | EnumElement Node Node
  | StructElement Node Node
  | CastToType Type Node
  | CastToIdentifier Node Node
  | LtOp Node Node
  | GtOp Node Node
  | LeqOp Node Node
  | GeqOp Node Node
  | EqOp Node Node
  | NeqOp Node Node
  | AndOp Node Node
  | OrOp Node Node
  | NotOp Node
  | AddOp Node Node
  | AddEqOp Symbol Node
  | SubOp Node Node
  | SubEqOp Symbol Node
  | MulOp Node Node
  | MulEqOp Symbol Node
  | DivOp Node Node
  | DivEqOp Symbol Node
  | ModOp Node Node
  | SizeofExpr Node
  | SizeofType Type
  | Reference Node
  | Dereference Node
  | ArrayAccess Node Node
  | Syscall [Node]
  | Print Node
  deriving (Show, Eq)
