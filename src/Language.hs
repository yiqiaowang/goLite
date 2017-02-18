module Language where

-- The layout of the entire program
data Program
      = Program Package [All]
      deriving Show

-- Packages, Identifiers, and Types are all strings
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

-- All Statements
data All
      = Stmt Stmt
      | Function Identifier [Parameter] (Maybe Type) [Stmt]
      deriving Show

-- Statements that can be declared inside blocks
data Stmt
      = VarDec Identifier (Maybe Type) Expression
      | TypeDec Identifier Type
      | StructDec Identifier [[Identifer] Type]
      | ArrayDec Identifier Int Type
      | SliceDec Identifier Type
      | ExpStmt Expression
      | Assign [Identifier] [Expression]
      | EmptyStmt
      deriving Show

