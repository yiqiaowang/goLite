module Language where

-- The layout of the entire program
data Program
      = Program Package [All]
      deriving (Eq, Show)

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
      deriving (Eq, Show)

-- Parameter data type (List of identifiers with an associated type)
data Parameter
      = Parameter [Identifier] Type
      deriving (Eq, Show)

-- Clauses in Switch statment
data Clause
      = Case [Expression] [Stmt]
      | Default [Stmt]
      deriving (Eq, Show)

-- Recursive If Statement
data IfStmt
      = IfStmt (Maybe SimpleStmt) Expression [Stmt] (Maybe (Either IfStmt [Stmt]))
      deriving (Eq, Show)

-- All Statements
data All
      = Stmt Stmt
      | Function Identifier [Parameter] (Maybe Type) [Stmt]
      deriving (Eq, Show)

-- Statements that can be declared inside blocks
data Stmt
      = VarDec [Identifier] (Maybe Type) [Expression]
      | TypeDec Identifier Type
      | Struct Identifier [([Identifier], Type)]
      | Array Identifier Int Type
      | Slice Identifier Type
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
      deriving (Eq, Show)

-- Simple statements
data SimpleStmt
      = EmptyStmt
      | ExpStmt Expression
      | Incr Identifier
      | Decr Identifier
      | Assign [Identifier] [Expression]
      | ShortVarDec [Identifier] [Expression]
      deriving (Eq, Show)

-- Expressions
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
      | Equals Expression Expression
      | NotEquals Expression Expression
      | LThan Expression Expression
      | LEThan Expression Expression
      | GThan Expression Expression
      | GEThan Expression Expression
      | Plus Expression Expression
      | Minus Expression Expression      
      | Mul Expression Expression
      | Div Expression Expression
      | Mod Expression Expression
      | BitAnd Expression Expression
      | BitOr Expression Expression
      | BitXOr Expression Expression
      | BitAndNot Expression Expression
      | LShift Expression Expression
      | RShift Expression Expression
      | Call Identifier [Expression]
      | Append Identifier Expression
      | Index Identifier Expression
      | Field Identifier Identifier
      deriving (Eq, Show)
