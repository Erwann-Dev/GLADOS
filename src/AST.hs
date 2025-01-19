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
  deriving (Show)

data BasicType
  = U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | Void
  | Pointer Type
  deriving (Show)

data Node
  = IntegerValue Int
  | FloatValue Float
  | ArrayValue [Node]
  | Identifier Symbol
  | VariableInitialization Symbol Type Node
  | Assignment Symbol Node
  | Block [Node]
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
  | LessThanOperator Node Node
  | GreaterThanOperator Node Node
  | LessThanOrEqualOperator Node Node
  | GreaterThanOrEqualOperator Node Node
  | EqualOperator Node Node
  | NotEqualOperator Node Node
  | AndOperator Node Node
  | OrOperator Node Node
  | NotOperator Node
  | PlusOperator Node Node
  | PlusEqualOperator Symbol Node
  | MinusOperator Node Node
  | MinusEqualOperator Symbol Node
  | MultiplyOperator Node Node
  | MultiplyEqualOperator Symbol Node
  | DivideOperator Node Node
  | DivideEqualOperator Symbol Node
  | ModuloOperator Node Node
  | SizeofOfExpressionOperator Node
  | SizeofOfTypeOperator Type
  | ReferenceOperator Node
  | DereferenceOperator Node
  | ArrayAccess Node Node
  | Syscall [Node]
  deriving (Show)
