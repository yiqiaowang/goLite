module Language.TypedLanguage where


import Language.Operators
import Language.Common


-- TExpression couples an expression with its evaluated type.
-- The type information will be used in the later phases of the compiler.
type TExpression = (TExpression', Type)


-- The layout of the entire program
data TProgram =
  TProgram Package [TAll]
  deriving (Eq, Show)


-- All Statements
data TAll
  = TTopDec TTopLevel
  | TFunction FunctionName
             [Parameter]
             (Maybe Type)
             [TStmt]
  deriving (Eq, Show)

-- Top Level Declarations
data TTopLevel
  = TVarDec TVariable
  | TVarDecList [TVariable]
  | TTypeDec TypeName
  | TTypeDecList [TypeName]
  deriving (Eq, Show)

-- Variable declaration
data TVariable =
  TVariable [Identifier]
            (Maybe Type)
            [TExpression]
  deriving (Eq, Show)

-- Statements that can be declared inside blocks
data TStmt
  = TStmtDec TTopLevel
  | TSimpleStmt TSimpleStmt
  | TPrint [TExpression]
  | TPrintln [TExpression]
  | TReturn (Maybe TExpression)
  | TIf TIfStmt
  | TSwitch TSimpleStmt
           (Maybe TExpression)
           [TClause]
  | TInfinite [TStmt]
  | TWhile TExpression [TStmt]
  | TFor TSimpleStmt
        (Maybe TExpression)
        TSimpleStmt
        [TStmt]
  | TBlock [TStmt]
  | TBreak
  | TContinue
  deriving (Eq, Show)

-- Simple statements
data TSimpleStmt
  = TStmtFuncCall TFunctionCall
  | TIncr Identifier
  | TDecr Identifier
  | TAssign [Identifier] [TExpression]
  | TShortBinary BinaryOpEq Identifier TExpression
  | TShortVarDec [Identifier] [TExpression]
  | TEmptyStmt
  deriving (Eq, Show)

-- For function calls (can be statements or expressions)
data TFunctionCall =
  TFunctionCall FunctionName [TExpression]
  deriving (Eq, Ord, Show)

-- Recursive If Statement
data TIfStmt =
  TIfStmt TSimpleStmt
         TExpression
         [TStmt]
         TIfStmtCont
  deriving (Eq, Show)

data TIfStmtCont =
  TIfStmtCont (Maybe (Either TIfStmt [TStmt]))
  deriving (Eq, Show)

-- Clauses in Switch statment
data TClause
  = TCase [TExpression] [TStmt]
  | TDefault [TStmt]
  deriving (Eq, Show)

-- Expressions
data TExpression'
  = TBrack TExpression
  | TId Identifier
  | TLiteral Literal
  | TExprFuncCall TFunctionCall
  | TAppend Identifier TExpression
  | TUnary UnaryOp TExpression
  | TBinary BinaryOp TExpression TExpression
  deriving (Eq, Ord, Show)
