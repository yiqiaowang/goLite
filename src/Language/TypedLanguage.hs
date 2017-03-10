module Language.TypedLanguage where


import Language.Operators
import Language.Common


-- TExpression couples an expression with its evaluated type.
-- The type information will be used in the later phases of the compiler.
type TExpression = (TExpression', TType)


-- The layout of the entire program
data TProgram =
  TProgram Package [TAll]
  deriving (Eq, Show)


-- All Statements
data TAll
  = TTopDec TTopLevel
  | TFunction FunctionName
             [TParameter]
             (Maybe TType)
             [TStmt]
  deriving (Eq, Show)

-- Top Level Declarations
data TTopLevel
  = TVarDec TVariable
  | TVarDecList [TVariable]
  | TTypeDec TTypeName
  | TTypeDecList [TTypeName]
  deriving (Eq, Show)

-- Variable declaration
data TVariable =
  TVariable [TIdentifier]
            (Maybe TType)
            [TExpression]
  deriving (Eq, Show)

-- Type aliasing
data TTypeName =
  TTypeName TType TType
  deriving (Eq, Show)

-- Where the First Type is the variable name and the second
-- Type is the Associated Type
data TIdentifier
  = TIdOrType String
  | TIdArray String [TExpression]
  | TIdField [TIdentifier]
  deriving (Eq, Ord, Show)

data TType
  = TAlias String
  | TArray TType Int
  | TSlice TType
  | TStruct [([TIdentifier], TType)]
  -- TODO: should we add the input types -> output type into the func constructor
  | TFunc
  | TBool
  deriving (Eq, Ord, Show)

-- Parameter data type (List of identifiers with an associated type)
data TParameter =
  TParameter [TIdentifier] TType
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
  | TIncr TIdentifier
  | TDecr TIdentifier
  | TAssign [TIdentifier] [TExpression]
  | TShortBinary BinaryOpEq TIdentifier TExpression
  | TShortVarDec [TIdentifier] [TExpression]
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
  | TId TIdentifier
  | TLiteral Literal
  | TExprFuncCall TFunctionCall
  | TAppend TIdentifier TExpression
  | TUnary UnaryOp TExpression
  | TBinary BinaryOp TExpression TExpression
  deriving (Eq, Ord, Show)
