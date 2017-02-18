module Language where

-- The layout of the entire program
data Program
      = Program Package [Stmt]
      deriving Show

type Package = String
type Identifier = String
type Type = String

-- Literal values
data Literal
      = LitInt Int
      | LitFloat64 Float
      | LitBool Bool
      | LitRune Char
      | LitString String
      deriving Show

-- Parameter data type (List of identifiers with an associated type identifier)
data Parameter
      = Parameter [Identifier] Type
      deriving Show

-- Statements
data Stmt
      = VarDec Identifier (Maybe Type) Expression
      | TypeDec Identifier Type
      | StructDec Identifier [[Identifer] Type]
      | ArrayDec Identifier Int Type
      | SliceDec Identifier Type
      | Function Identifier [Parameter] (Maybe Type) [Stmt]
      deriving Show

