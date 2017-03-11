module Language.Language where


import Language.Operators
import Language.Common


-- The layout of the entire program
data Program =
  Program Package [All]
  deriving (Eq, Show)

-- All Statements
data All
  = TopDec TopLevel
  | Function FunctionName
             [Parameter]
             (Maybe Type)
             [Stmt]
  deriving (Eq, Show)

-- Top Level Declarations
data TopLevel
  = VarDec Variable
  | VarDecList [Variable]
  | TypeDec TypeName
  | TypeDecList [TypeName]
  deriving (Eq, Show)

-- Variable declaration
data Variable =
  Variable [Identifier]
           (Maybe Type)
           [Expression]
  deriving (Eq, Show)

-- Statements that can be declared inside blocks
data Stmt
  = StmtDec TopLevel
  | SimpleStmt SimpleStmt
  | Print [Expression]
  | Println [Expression]
  | Return (Maybe Expression)
  | If IfStmt
  | Switch SimpleStmt
           (Maybe Expression)
           [Clause]
  | Infinite [Stmt]
  | While Expression [Stmt]
  | For SimpleStmt
        (Maybe Expression)
        SimpleStmt
        [Stmt]
  | Block [Stmt]
  | Break
  | Continue
  deriving (Eq, Show)

-- Simple statements
data SimpleStmt
  = StmtFuncCall FunctionCall
  | Incr Identifier
  | Decr Identifier
  | Assign [Identifier] [Expression]
  | ShortBinary BinaryOpEq Identifier Expression
  | ShortVarDec [Identifier] [Expression]
  | EmptyStmt
  deriving (Eq, Show)

-- For function calls (can be statements or expressions)
data FunctionCall =
  FunctionCall FunctionName [Expression]
  deriving (Eq, Ord, Show)

-- Recursive If Statement
data IfStmt =
  IfStmt SimpleStmt
         Expression
         [Stmt]
         IfStmtCont
  deriving (Eq, Show)

data IfStmtCont =
  IfStmtCont (Maybe (Either IfStmt [Stmt]))
  deriving (Eq, Show)

-- Clauses in Switch statment
data Clause
  = Case [Expression] [Stmt]
  | Default [Stmt]
  deriving (Eq, Show)

-- Expressions
data Expression
  = Brack Expression
  | Id Identifier
  | Literal Literal
  | ExprFuncCall FunctionCall
  | Append Identifier Expression
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  deriving (Eq, Ord, Show)
