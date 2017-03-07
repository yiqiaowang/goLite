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
  | TypeNotPresentError { typeReceived :: Maybe Type
                       ,  typeOptions :: [Type]}
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
  typeCheckList :: SymbolTable
                -> [a]
                -> Either (Maybe Type, SymbolTable) TypeCheckError
  typeCheckList symtbl [] = Left (Nothing, symtbl)
  typeCheckList symtbl (x:xs) =
    case typeCheck symtbl x of
      Left (_, symtbl') -> typeCheckList symtbl' xs
      Right err -> Right err
  typeCheckElemOf
    :: SymbolTable
    -> a
    -> [Type]
    -> Either (Maybe Type, SymbolTable) TypeCheckError
  typeCheckElemOf symtbl a types =
    case typeCheck symtbl a of
      Left (Nothing, _) -> Right $ TypeNotPresentError Nothing types
      Left (Just t, symtbl') ->
        if t `elem` types
          then Left (Nothing ,symtbl')
          else Right $ TypeNotPresentError (Just t) types
      Right err -> Right err
  typeCheckListElemOf
    :: SymbolTable
    -> [a]
    -> [Type]
    -> Either (Maybe Type, SymbolTable) TypeCheckError
  typeCheckListElemOf symtbl [] _ = Left (Nothing, symtbl)
  typeCheckListElemOf symtbl (a:as) types =
    case typeCheckElemOf symtbl a types of
      Left (_, symtbl) -> typeCheckListElemOf symtbl as types
      Right err -> Right err


instance TypeCheckable Program where
  typeCheck symtbl (Program prog alls) =
    case typeCheckList symtbl alls of
      Left (Nothing, symtbl') -> Left (Nothing, symtbl')
      Right err -> Right err

instance TypeCheckable All where
  typeCheck symtbl (TopDec stmt) = typeCheck symtbl stmt
  -- For a function F to be type correct, the following need to be satisfied:
  -- Each statement in the function body must be correct.
  -- If the function returns a value, each execution path should contain a well typed
  -- return statement. [ Do we interpret this as is or as go's spec specifies? ]
  -- Futhermore, each parameter specified in the definition gets added to the stack frame
  -- of the function body.
  typeCheck symtbl (Function iden params ret stmts)
    -- Add the function to the current frame, check for failure
   =
    case addEntry symtbl iden (Entry CategoryVariable $ Just Func) of
      Left symtbl'
        -- Create a new frame, and type check the body of the function
        -- with the newly created frame
       ->
        case typeCheckList (newFrame symtbl') stmts of
          Left (_, symtbl'')
            -- Check to see that all returns return the correct types
            -- If so, pop the stack frame, and return a new one
           ->
            case typeCheckReturns symtbl'' stmts ret of
              Nothing -> Left (Nothing, snd $ popFrame symtbl'')
              Just err -> Right err
          Right err -> Right err
      Right err -> Right $ SymbolTableError err

-- Type check returns on a block of statements. only checks outer-most return statements.
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
-- Type check returns for IFs
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont Nothing)):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Left (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing -> typeCheckReturns (snd $ popFrame symtbl') xs ret
        Just err -> Just err
    Right err -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Left ifstmt)))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Left (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          typeCheckReturns (snd $ popFrame symtbl') ((If ifstmt) : xs) ret
        Just err -> Just err
    Right err -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Right stmts')))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Left (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          case typeCheckList (newFrame $ snd $ popFrame symtbl') stmts' of
            Left (_, symtbl'') -> Nothing
            Right err -> Just err
        Just err -> Just err
    Right err -> Just err
-- Type Check returns for Infinite
typeCheckReturns symtbl ((Infinite stmts):xs) ret =
  case typeCheckReturns (newFrame symtbl) stmts ret of
    Nothing -> typeCheckReturns symtbl xs ret
    Just err -> Just err
-- Type check return for While
typeCheckReturns symtbl ((While _ stmts):xs) ret =
  case typeCheckReturns (newFrame symtbl) stmts ret of
    Nothing -> typeCheckReturns symtbl xs ret
    Just err -> Just err
-- Type Check returns for For
typeCheckReturns symtbl ((For _ _ _ stmts):xs) ret =
  case typeCheckReturns (newFrame symtbl) stmts ret of
    Nothing -> typeCheckReturns symtbl xs ret
    Just err -> Just err
-- Type Check returns for Clauses
typeCheckReturns symtbl ((Switch _ _ clauses):xs) ret =
  case typeCheckClauseReturns symtbl clauses ret of
    Nothing -> typeCheckReturns symtbl xs ret
    Just err -> Just err
-- Type Check returns other cases
typeCheckReturns symtbl (_:xs) t = typeCheckReturns symtbl xs t

typeCheckClauseReturns :: SymbolTable
                       -> [Clause]
                       -> Maybe Type
                       -> Maybe TypeCheckError
typeCheckClauseReturns symtbl [] ret = Nothing
typeCheckClauseReturns symtbl ((Case _ stmts):cs) ret =
  case typeCheckReturns (newFrame symtbl) stmts ret of
    Nothing -> typeCheckClauseReturns symtbl cs ret
    Just err -> Just err
typeCheckClauseReturns symtbl ((Default stmts):cs) ret =
  case typeCheckReturns (newFrame symtbl) stmts ret of
    Nothing -> typeCheckClauseReturns symtbl cs ret
    Just err -> Just err

assertTypeEqual :: Maybe Type -> Maybe Type -> Bool
assertTypeEqual expected actual =
  if expected == actual
    then True
    else False

-- Parameters.  Adds a parameter list to the symbol table for use in
-- function body.
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

--
instance TypeCheckable TopLevel where
  typeCheck symtbl (VarDec var) = typeCheck symtbl var
  typeCheck symtbl (VarDecList vars) = typeCheckList symtbl vars
  typeCheck symtbl (TypeDec typename) = typeCheck symtbl typename
  typeCheck symtbl (TypeDecList typenames) = typeCheckList symtbl typenames

--
instance TypeCheckable Stmt where
  typeCheck symtbl (SimpleStmt ss) = typeCheck symtbl ss
  -- Base types for all expressions must be one of the primative types
  typeCheck symtbl (Print exprs) =
    typeCheckListElemOf
      symtbl
      exprs
      [ (Alias "int")
      , (Alias "float64")
      , (Alias "bool")
      , (Alias "string")
      , (Alias "rune")
      ]
  typeCheck symtbl (Println exprs) =
    typeCheckListElemOf
      symtbl
      exprs
      [ (Alias "int")
      , (Alias "float64")
      , (Alias "bool")
      , (Alias "string")
      , (Alias "rune")
      ]
  -- Returns are typechecked seperatly in the function typeCheckReturns
  -- which also type checks that the return returns the expected type
  typeCheck symtbl (Return _) = Left (Nothing, symtbl)


  typeCheck symtbl (Block stmts) =
    case typeCheckList (newFrame symtbl) stmts of
      Left (_, symtbl') -> Left (Nothing, snd $ popFrame symtbl')
      Right err -> Right err

  -- Breaks and continues are trivially well typed
  typeCheck symtbl (Break) = Left (Nothing, symtbl)
  typeCheck symtbl (Continue) = Left (Nothing, symtbl)


  typeCheck symtbl (If ifstmt) = typeCheck symtbl ifstmt
  -- typeCheck symtbl (For maybe_ss maybe_expr maybe_ss' stmts) =
  typeCheck symtbl (Infinite stmts) = typeCheckList symtbl stmts
  -- typeCheck symtbl (While expr stmts) =
  -- typeCheck symtbl (Switch maybe_ss maybe_expr clauses) =

instance TypeCheckable IfStmt where
  typeCheck symtbl (IfStmt Nothing expr stmts fac) =
    case typeCheckElemOf symtbl expr [(Alias "bool")] of
      Left (Nothing, symtbl') -> case typeCheckList symtbl' stmts of
                                   Left (_, symtbl'') -> typeCheck symtbl'' fac
                                   Right err -> Right err
      Right err -> Right err

instance TypeCheckable IfStmtCont


instance TypeCheckable TypeName where
  typeCheck symtbl (TypeName (Alias typename) typeval) =
    case addEntry symtbl (IdOrType typename) (Entry CategoryType $ Just typeval) of
      Left symtbl' -> Left (Nothing, symtbl')
      Right err -> Right $ SymbolTableError err

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

--
instance TypeCheckable SimpleStmt where
  typeCheck symtbl (StmtFuncCall funcCall) = typeCheck symtbl funcCall
  -- typeCheck symtbl (Incr iden) =
  -- typeCheck symtbl (Decr iden) =
  -- typeCheck symtbl (Assign idens exprs) =
  -- typeCheck symtbl (ShortBinary op iden expr) =
  -- typeCheck symtbl (ShortVarDec idens exprs) =



instance TypeCheckable FunctionCall

--
instance TypeCheckable Expression where
  typeCheck symtbl (Id i) = typeCheck symtbl i
  typeCheck symtbl (Brack expr) = typeCheck symtbl expr
  typeCheck symtbl (Literal lit) = typeCheck symtbl lit
  -- typeCheck symtbl (FuncCall iden exprs) =
  -- typeCheck symtbl (Append iden expr) =
  -- typeCheck symtbl (Unary op expr) =
  -- typeCheck symtbl (Binary op expr expr') =

instance TypeCheckable Literal
