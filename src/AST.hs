module AST where

data Expr
  = Number Int
  | Boolean Bool
  | List [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  deriving (Show)

data Value
  = NumVal Int
  | BoolVal Bool
  | ListVal [Value]
  deriving (Eq)

instance Show Value where
  show (NumVal n) = show n
  show (BoolVal b) = if b then "#t" else "#f"
  show (ListVal xs) = "(" ++ unwords (map show xs) ++ ")"
  show _ = ""

instance Ord Value where
  compare (NumVal n1) (NumVal n2) = compare n1 n2
  compare _ _ = error "Cannot compare"
