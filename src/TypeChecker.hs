module TypeChecker
  ( TypeCheckError(..)
  , typeCheck
  , typeCheckList
  ) where

import Language
import SymbolTable

type ExpectedType = Maybe Type

type ActualType = Maybe Type

data TypeCheckError
  = SymbolTableError { symbolTableError :: SymbolTableError}
  | TypeMismatchError { expectedType :: ExpectedType
                     ,  actualType :: ActualType}
  deriving (Eq, Show)

-- This typeclass takes something from Lanugage and a SymbolTable and returns either a
-- (Maybe Type, SymbolTable) or a TypeCheckError. This is done so that all language data types can
-- instantiate the TypeCheckable class. Example. Both a Statement and an Expression can
-- and should be typechecked. However, the expression should return a type whereas the statement
-- should just return the fact that it is type correct; it makes no sense to type statements.
-- So a statement would return Left (Nothing, SymTbl) on success, or Right TypeCheckError on type error
-- An expression would return Left (Just Type, SymbTbl) on success and Right TypeCheckError on failure.
-- Since we build up the symbolTable while traversing the AST, we must return the update symbol table.
class TypeCheckable a where
  typeCheck :: SymbolTable
            -> a
            -> Either (Maybe Type, SymbolTable) TypeCheckError
  typeCheck symtbl a = Left (Nothing, symtbl)
  typeCheckList :: [a]
                -> SymbolTable
                -> Either (Maybe Type, SymbolTable) TypeCheckError
  typeCheckList [] symtbl = Left (Nothing, symtbl)
  typeCheckList (x:xs) symtbl =
    case typeCheck symtbl x of
      Left (_, symtbl') -> typeCheckList xs symtbl'
      Right err -> Right err

instance TypeCheckable Program where
  typeCheck symtbl (Program prog alls) =
    case typeCheckList alls symtbl of
      Left (Nothing, symtbl') -> Left (Nothing, symtbl')
      Right err -> Right err

instance TypeCheckable All where
  typeCheck symtbl (Stmt stmt) = typeCheck symtbl stmt
  -- For a function F to be type correct, the following need to be satisfied:
  -- Each statement in the function body must be correct.
  -- If the function returns a value, each execution path should contain a well typed
  -- return statement. [ Do we interpret this as is or as go's spec specifies? ]
  -- Futhermore, each parameter specified in the definition gets added to the stack frame
  -- of the function body.
  typeCheck symtbl (Function iden params ret stmts) =
    case addEntry symtbl iden (Entry CategoryVariable $ Just Func) of
      Left symtbl' ->
        case typeCheckBlock (newFrame symtbl') stmts of
          Left symtbl'' ->
            case typeCheckReturns symtbl'' stmts ret of
              Nothing -> Left (Nothing, snd $ popFrame symtbl'')
              Just err -> Right err
          Right err -> Right err
      Right err -> Right $ SymbolTableError err

-- TypeChecks a block of statements
-- Does not return a type since a block of statements does not return a type
typeCheckBlock :: SymbolTable -> [Stmt] -> Either SymbolTable TypeCheckError
typeCheckBlock symtbl [] = Left symtbl
typeCheckBlock symtbl xs =
  case typeCheckList xs symtbl of
    Left (_, symtbl') -> Left symtbl'
    Right err -> Right err

-- Type check returns on a block of statements. only checks outer-most return statements.
-- To check the returns inside an IF statment, for example, one should run
-- this function on the body of the if statment when typechecking if statements.
-- The behaviour on empty list may change depending on how we interpret
-- "There must be a return on all execution paths (GoLite spec vs Go Spec)"
typeCheckReturns :: SymbolTable -> [Stmt] -> Maybe Type -> Maybe TypeCheckError
typeCheckReturns symtbl [] _ = Nothing
typeCheckReturns symtbl (Return Nothing:xs) t =
  case t of
    Nothing -> typeCheckReturns symtbl xs t
    t' -> Just (TypeMismatchError t' Nothing)
typeCheckReturns symtbl (Return (Just expr):xs) t =
  case typeCheck symtbl expr of
    Left (t', symtbl') ->
      if assertTypeEqual t t'
        then typeCheckReturns symtbl' xs t
        else Just (TypeMismatchError t t')
    Right err -> Just (err)
typeCheckReturns symtbl (_:xs) t = typeCheckReturns symtbl xs t

assertTypeEqual :: Maybe Type -> Maybe Type -> Bool
assertTypeEqual expected actual =
  if expected == actual
    then True
    else False

-- Parameters.  Adds a parameter list to the symbol table.
instance TypeCheckable Parameter where
  typeCheck symtbl (Parameter [] t) = Left (Nothing, symtbl)
  typeCheck symtbl (Parameter ids t) =
    case symbolTableFuncParam (Left symtbl) ids t of
      (Left symtbl) -> Left (Nothing, symtbl)
      (Right err) -> (Right (SymbolTableError err))

symbolTableFuncParam
  :: Either SymbolTable SymbolTableError
  -> [Identifier]
  -> Type
  -> Either SymbolTable SymbolTableError
symbolTableFuncParam (Left symtbl) [] _ = Left symtbl
symbolTableFuncParam (Left symtbl) (i:is) t =
  symbolTableFuncParam
    (addEntry symtbl i (Entry CategoryVariable $ Just t))
    is
    t
symbolTableFuncParam (Right err) _ _ = (Right err)

instance TypeCheckable Stmt where
  typeCheck symtbl (VarDec var) = typeCheck symtbl var
  typeCheck symtbl (Return _) = Left (Nothing, symtbl)

instance TypeCheckable Identifier where
  typeCheck symtbl i =
    case lookupIdentifier symtbl i of
      Left (Entry _ t) -> Left (t, symtbl)
      Right err -> Right $ SymbolTableError err

instance TypeCheckable Variable where
  typeCheck symtbl (Variable [] maybe_type []) = Left (Nothing, symtbl)
  typeCheck symtbl (Variable (i:is) maybe_type (e:es)) =
    case addEntry symtbl i (Entry CategoryVariable maybe_type) of
      Left symtbl' -> typeCheck symtbl' (Variable is maybe_type es)
      Right err -> Right $ SymbolTableError err

instance TypeCheckable SimpleStmt

instance TypeCheckable Expression where
  typeCheck symtbl (Id i) = typeCheck symtbl i
