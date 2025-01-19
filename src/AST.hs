module AST
  ( Node (..),
    Type (..),
    BasicType (..),
    TypeSize,
  )
where

type TypeSize = Int

data Type = Type
  { basic_type :: BasicType,
    mutable :: Bool
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
  | VariableInitialization Node Type Node
  | Assignment Node Node
  | Block [Node]
  | Return Node
  | If Node Node (Maybe Node)
  | While Node Node
  | For (Maybe Node) Node (Maybe Node) Node
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
  | PlusEqualOperator Node Node
  | MinusOperator Node Node
  | MinusEqualOperator Node Node
  | MultiplyOperator Node Node
  | MultiplyEqualOperator Node Node
  | DivideOperator Node Node
  | DivideEqualOperator Node Node
  | ModuloOperator Node Node
  | SizeofOfExpressionOperator Node
  | SizeofOfTypeOperator Type
  | ReferenceOperator Node
  | DereferenceOperator Node
  | ArrayAccess Node Node
  | Syscall [Node]
  | Identifier String
  deriving (Show)
