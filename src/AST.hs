module AST
  ( Expr(..)
  , Value(..)
  , Env
  ) where

type Symbol = String
type Env = [(Symbol, Value)]

data Expr
  = Number Int
  | Boolean Bool
  | List [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | If Expr Expr Expr
  | Var Symbol
  | Define String Expr
  | Lam [Symbol] Expr
  | Apply Expr [Expr]
  deriving (Show, Eq)

data Value
  = NumVal Int
  | BoolVal Bool
  | ListVal [Value]
  | Closure [Symbol] Expr Env
  | Null
  deriving (Eq)

instance Show Value where
  show (NumVal n) = show n
  show (BoolVal b) = if b then "#t" else "#f"
  show (ListVal xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Closure{}) = "<closure>"
  show _ = ""

instance Ord Value where
  compare (NumVal n1) (NumVal n2) = compare n1 n2
  compare _ _ = error "Cannot compare"
