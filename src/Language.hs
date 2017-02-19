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
      = Int Int
      | Float64 Float
      | Bool Bool
      | Rune Char
      | String String
      deriving Show

-- Parameter data type (List of identifiers with an associated type)
data Parameter
      = Parameter [Identifier] Type
      deriving Show

-- Clauses in Switch statment
data Clause
      = Case [Expression] [Stmt]
      | Default [Stmt]
      deriving Show

-- Recursive If Statement
data IfStmt
      = IfStmt (Maybe SimpleStmt) Expression [Stmt] (Maybe (Either IfStmt [Stmt]))
      deriving Show

-- All Statements
data All
      = Stmt Stmt
      | Function Identifier [Parameter] (Maybe Type) [Stmt]
      deriving Show

-- Statements that can be declared inside blocks
data Stmt
      = VarDec [Identifier] (Maybe Type) [Expression]
      | TypeDec Identifier Type
      | StructDec Identifier [[Identifer] Type]
      | ArrayDec Identifier Int Type
      | SliceDec Identifier Type
      | SimpleStmt SimpleStmt
      | Print Expression
      | Println Expression
      | Return (Maybe Expression)
      | If IfStmt
      | Switch (Maybe SimpleStmt) (Maybe Expression) [Clause]
      | Infinite [Stmt]
      | While Expression [Stmt]
      | For SimpleStmt Expression SimpleStmt [Stmt]
      | Break
      | Continue
      deriving Show

-- Simple statements
data SimpleStmt
      = EmptyStmt
      | ExpStmt Expression
      | Incr Identifier
      | Decr Identifier
      | Assign [Identifier] [Expression]
      | ShortVarDec [Identifier] [Expression]
      deriving Show

data Expression
      = Brack Expression
      | Id Identifier
      | Literal Literal
      | Positive Expression
      | Negative Expression
      | Not Expression
      | BitComplement Expression
      | Or Expression Expression
      | And Expression Expression
      | Equals Expression Expresssion
      | NotEquals Expression Expression
      | LThan Expression Expression
      | LEThan Expression Expression
      | GThan Expression Expression
      | GEThan Expression Expression