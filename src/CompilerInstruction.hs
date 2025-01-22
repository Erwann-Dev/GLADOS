module CompilerInstruction (
  Instruction(..),
) where

data Instruction
  = PushInt Int
  | PushFloat Float
  | PushNull
  | LoadVar String
  | StoreVar String
  | DefineVar String Bool
  | Pop
  | -- Stack frame management instructions
    PushFrame -- Create new stack frame
  | PopFrame -- Remove current stack frame
  | StoreLocal String -- Store in current frame
  | LoadLocal String -- Load from current frame
  | StoreGlobal String -- Store in global scope
  | LoadGlobal String -- Load from global scope
  -- Rest of instructions
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | And
  | Or
  | Equal
  | NotEqual
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | JumpIfFalse Int
  | Jump Int
  | Call Int
  | Return
  | MakeArray Int
  | LoadArray
  | Print
  deriving (Show, Eq)
