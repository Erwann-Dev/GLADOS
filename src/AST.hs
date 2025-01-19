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

data BasicType -- parsed
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
  = IntegerValue Int -- parsed
  | FloatValue Float -- parsed
  | ArrayValue [Node] -- parsed
  | VariableInitialization Symbol Type Node -- parsed
  | Assignment Symbol Node -- parsed (inline)
  | Block [Node] -- parsed?
  | Return Node -- parsed
  | If Node Node (Maybe Node) -- parsed
  | While Node Node -- parsed
  | For (Maybe Node) Node (Maybe Node) Node -- parsed
  | FunctionDeclaration Type Node [(Type, Node)] Node
  | FunctionCall Node [Node]
  | EnumDeclaration Node [Node]
  | StructDeclaration Node [(Type, Node)]
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
  | Identifier String
  deriving (Show)
