{- | The 'AST' module defines the abstract syntax tree (AST) for our simple
expression language. It includes the representation of expressions,
values, and environments, and provides the basic data structures to
evaluate and manipulate the expressions.
-}
module AST (
  -- * Types
  Expr (..),
  Value (..),
  Symbol,
  Env,
) where

{- | A 'Symbol' is simply a type alias for a 'String'. It represents variable
names or function names within expressions.
-}
type Symbol = String

{- | An 'Env' (short for "Environment") is a list of symbol-value pairs, where
each symbol is associated with a value. It represents the environment in which
expressions are evaluated.
-}
type Env = [(Symbol, Value)]

{- | The 'Expr' data type represents various types of expressions in the language.
It can be a number, a boolean, a list, or more complex expressions like
arithmetic operations, comparisons, conditionals, variable lookups, function
definitions, and applications.
-}
data Expr
  = -- | A numeric literal.
    Number Int
  | -- | A boolean literal.
    Boolean Bool
  | -- | A list of expressions.
    List [Expr]
  | -- | Addition of two expressions.
    Add Expr Expr
  | -- | Subtraction of two expressions.
    Sub Expr Expr
  | -- | Multiplication of two expressions.
    Mul Expr Expr
  | -- | Division of two expressions.
    Div Expr Expr
  | -- | Equality comparison between two expressions.
    Eq Expr Expr
  | -- | Inequality comparison between two expressions.
    Neq Expr Expr
  | -- | Greater-than comparison between two expressions.
    Gt Expr Expr
  | -- | Less-than comparison between two expressions.
    Lt Expr Expr
  | -- | A conditional expression: if (condition) then (true branch) else (false branch).
    If Expr Expr Expr
  | -- | A variable reference.
    Var Symbol
  | -- | A variable definition, associating a symbol with an expression.
    Define String Expr
  | -- | A lambda expression with a list of parameters and a body.
    Lam [Symbol] Expr
  | -- | Function application with an expression as the function and a list of arguments.
    Apply Expr [Expr]
  deriving
    ( Show
    , -- | Automatically deriving 'Show' and 'Eq' for displaying and comparing expressions.
      Eq
    )

{- | The 'Value' data type represents the result of evaluating an expression.
A value can be a number, a boolean, a list, a closure (for functions), or
a special value 'Null'.
-}
data Value
  = -- | A numerical value.
    NumVal Int
  | -- | A boolean value.
    BoolVal Bool
  | -- | A list of values.
    ListVal [Value]
  | -- | A closure, which is a function consisting of parameters, a body, and an environment.
    Closure [Symbol] Expr Env
  | -- | A special value representing an empty or undefined result.
    Null
  deriving
    ( -- | Automatically deriving 'Eq' for comparing values.
      Eq
    )

{- | The 'Show' instance for 'Value' provides a way to display values as strings.
Numeric values are shown as numbers, boolean values are shown as '#t' (true)
or '#f' (false), lists are displayed with their elements in parentheses, and
closures are represented as "<procedure>".
-}
instance Show Value where
  show (NumVal n) = show n
  show (BoolVal b) = if b then "#t" else "#f"
  show (ListVal xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Closure{}) = "<procedure>"
  show _ = ""

{- | The 'Ord' instance for 'Value' defines an ordering for values. The only
values that can be compared are 'NumVal' instances. Comparisons between
non-numeric values result in an error.
-}
instance Ord Value where
  compare (NumVal n1) (NumVal n2) = compare n1 n2
  compare _ _ = error "Cannot compare non-numeric values"
