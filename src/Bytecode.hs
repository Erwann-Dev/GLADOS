module Bytecode
  ( Bytecode,
    Instruction (..),
  )
where

data Instruction
  = Test
  deriving (Show)

type Bytecode = [Instruction]
