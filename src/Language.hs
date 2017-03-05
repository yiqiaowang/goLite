module Language where

-- The layout of the entire program
data Program
      = Program Package [All]
      deriving (Eq, Show)

-- Packages, Identifiers, and Types are all strings
type Package = String


data Identifier = IdOrType String
                | IdArray String [Expression]
                | IdField [Identifier]
                deriving (Eq, Ord, Show)
data Type
  = Alias String
  | Array Type Int
  | Slice Type
  | Struct [([Identifier], Type)]
  -- TODO: should we add the input types -> output type into the func constructor
  | Func
  | Bool
  deriving (Eq, Show)


-- Literal values
data Literal
      = Int' Int
      | Float64 Float
      -- | Bool Bool
      | Rune Integer
      | String String
      | Raw String
      deriving (Eq, Ord, Show)

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

data Variable
      = Variable [Identifier] (Maybe Type) [Expression]
      deriving (Eq, Show)

data TypeName 
      = TypeName Identifier Type
      deriving (Eq, Show)


-- Statements that can be declared inside blocks
data Stmt
      = VarDec Variable
      | VarDecList [Variable]
      | TypeDec TypeName
      | TypeDecList [TypeName]
      | SimpleStmt SimpleStmt
      | Print [Expression]
      | Println [Expression]
      | Return (Maybe Expression)
      | If IfStmt
      | Switch (Maybe SimpleStmt) (Maybe Expression) [Clause]
      | Infinite [Stmt]
      | While Expression [Stmt]
      | For (Maybe SimpleStmt) (Maybe Expression) (Maybe SimpleStmt) [Stmt]
      | Block [Stmt]
      | Break
      | Continue
      deriving (Eq, Show)

-- Simple statements
data SimpleStmt
      = ExprStmt Expression
      | Incr Identifier
      | Decr Identifier
      | Assign [Identifier] [Expression]
      | ShortBinary BinaryOpEq Identifier Expression
      | ShortVarDec [Identifier] [Expression]
      deriving (Eq, Show)

data BinaryOpEq
      = PlusEq
      | MinusEq
      | MulEq
      | DivEq
      | ModEq
      | BitAndEq
      | BitOrEq
      | BitXorEq
      | BitLShiftEq
      | BitRShiftEq
      | BitClearEq
      deriving (Eq, Show)


-- Expressions
data Expression
      = Brack Expression
      | Id Identifier
      | Literal Literal
      | FuncCall Identifier [Expression]
      | Append Identifier Expression
      | Unary UnaryOp Expression
      | Binary BinaryOp Expression Expression
      deriving (Eq, Ord, Show)

data UnaryOp
      = Pos
      | Neg
      | BoolNot
      | BitComplement
      deriving (Eq, Ord, Show)

data BinaryOp
      = Or
      | And
      | Equals
      | NotEquals
      | LThan
      | LEThan
      | GThan
      | GEThan
      | Add
      | Sub
      | Mult
      | Div
      | Mod
      | BitAnd
      | BitOr
      | BitXor
      | BitLShift
      | BitRShift
      | BitClear
      deriving (Eq, Ord, Show)
