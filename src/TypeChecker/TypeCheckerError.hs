module TypeChecker.TypeCheckerError where


import Language.Language
import Language.Common
import TypeChecker.SymbolTable


type ExpectedType = Maybe Type
type ActualType = Maybe Type


data TypeCheckError
  = SymbolTableError { symbolTableError :: SymbolTableError}
  | TypeMismatchError { expectedType :: ExpectedType
                     ,  actualType :: ActualType}
  | TypeNotElementOfError { typeReceived :: Maybe Type
                         ,  typeOptions :: [Type]}
  | TypeNotCompatableError { typeLeft :: Maybe Type
                          ,  typeRight :: Maybe Type}
  | InvalidInitStatement
  | NoNewIdentifierError
  | AppendNotSliceError
  | ElementsNotComparableError
  | DefinitionNotFoundError
  | FuncCallArgNumError { expectedArgs :: [Type]
                       ,  receivedArgs :: [Expression]}
  | FuncCallArgTypeError { expectedArgType :: Type
                        ,  receivedArgType :: Maybe Type}
  | IllegalCastError { toType :: Type
                    ,  fromType :: Type}
  | EmptyCastError
  | InvalidTypeForOpError { invalidType :: Maybe Type}
  | UndefinedTypeError { undefinedType :: String}
  | VariableAsTypeError { variableAsTypeError :: String}
  | VariableDecError
  | DebugError
  | StructAccessError
  deriving (Eq, Show)
