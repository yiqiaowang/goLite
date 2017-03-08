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
  | TypeNotElementOfError { typeReceived :: Maybe Type
                         ,  typeOptions :: [Type]}
  | TypeCheckRawError { attemptedRawString :: String}
  deriving (Eq, Show)

-- This typeclass takes something from Lanugage and a SymbolTable and returns either a
-- (Maybe Type, SymbolTable) or a TypeCheckError. This is done so that all language data types can
-- instantiate the TypeCheckable class. Example. Both a Statement and an Expression can
-- and should be typechecked. However, the expression should return a type whereas the statement
-- should just return the fact that it is type correct; it makes no sense to type statements.
-- So a statement would return Right (Nothing, SymTbl) on success, or Left TypeCheckError on type error
-- An expression would return Right (Just Type, SymbTbl) on success and Left TypeCheckError on failure.
-- Since we build up the symbolTable while traversing the AST, we must return the update symbol table.
class TypeCheckable a where
  typeCheck
    :: SymbolTable
    -> a
    -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  typeCheck symtbl a = Right (Nothing, symtbl)
  typeCheckList
    :: SymbolTable
    -> [a]
    -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  typeCheckList symtbl [] = Right (Nothing, symtbl)
  typeCheckList symtbl (x:xs) =
    case typeCheck symtbl x of
      Right (_, symtbl') -> typeCheckList symtbl' xs
      Left err -> Left err
  typeCheckListNewFrame
    :: SymbolTable
    -> [a]
    -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  typeCheckListNewFrame symtbl xs =
    case typeCheckList (newFrame symtbl) xs of
      Right (_, symtbl') -> Right (Nothing, snd $ popFrame symtbl')
      Left err -> Left err
  typeCheckElemOf
    :: SymbolTable
    -> a
    -> [Type]
    -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  typeCheckElemOf symtbl a types =
    case typeCheck symtbl a of
      Right (Nothing, symtbl') ->
        Left (TypeNotElementOfError Nothing types, symtbl')
      Right (Just t, symtbl') ->
        if t `elem` types
          then Right (Nothing, symtbl')
          else Left (TypeNotElementOfError (Just t) types, symtbl')
      Left err -> Left err
  typeCheckListElemOf
    :: SymbolTable
    -> [a]
    -> [Type]
    -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  typeCheckListElemOf symtbl [] _ = Right (Nothing, symtbl)
  typeCheckListElemOf symtbl (a:as) types =
    case typeCheckElemOf symtbl a types of
      Right (_, symtbl) -> typeCheckListElemOf symtbl as types
      Left err -> Left err -- typeCheckListElemOf
  --   :: SymbolTable
  --   -> [a]
  --   -> [Type]
  --   -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
  -- typeCheckListElemOf symtbl xs types =
  --   case typeCheckListElemOf (newFrame symtbl) xs types of
  --     Right (_, symtbl') -> Right (Nothing, symtbl')
  --     Left err -> Left err

instance TypeCheckable Program where
  typeCheck symtbl (Program prog alls) =
    case typeCheckList symtbl alls of
      Right (Nothing, symtbl') -> Right (Nothing, symtbl')
      Left err -> Left err

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
      Right symtbl'
        -- Create a new frame, and type check the body of the function
        -- with the newly created frame
       ->
        case typeCheckList (newFrame symtbl') stmts of
          Right (_, symtbl'')
            -- Check to see that all returns return the correct types
            -- If so, pop the stack frame, and return a new one
           ->
            case typeCheckReturns symtbl'' stmts ret of
              Nothing -> Right (Nothing, snd $ popFrame symtbl'')
              Just err -> Left (err, symtbl'')
          Left err -> Left err
      Left err -> Left (SymbolTableError err, symtbl)

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
    Right (t', symtbl') ->
      if assertTypeEqual t t'
        then typeCheckReturns symtbl' xs t
        else Just (TypeMismatchError t t')
    Left (err, _) -> Just err
-- Type check returns for IFs
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont Nothing)):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Right (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing -> typeCheckReturns (snd $ popFrame symtbl') xs ret
        Just err -> Just err
    Left (err, _) -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Left ifstmt)))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Right (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          typeCheckReturns (snd $ popFrame symtbl') ((If ifstmt) : xs) ret
        Just err -> Just err
    Left (err, _) -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Right stmts')))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Right (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          case typeCheckList (newFrame $ snd $ popFrame symtbl') stmts' of
            Right (_, symtbl'') -> Nothing
            Left (err, _) -> Just err
        Just err -> Just err
    Left (err, _) -> Just err
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
  typeCheck symtbl (Parameter [] t) = Right (Nothing, symtbl)
  typeCheck symtbl (Parameter ids t) =
    case symbolTableFuncParam (Right symtbl) ids t of
      Right symtbl -> Right (Nothing, symtbl)
      Left err -> Left (SymbolTableError err, symtbl)

symbolTableFuncParam
  :: Either SymbolTableError SymbolTable
  -> [Identifier]
  -> Type
  -> Either SymbolTableError SymbolTable
symbolTableFuncParam (Right symtbl) [] _ = Right symtbl
symbolTableFuncParam (Right symtbl) (i:is) t =
  symbolTableFuncParam
    (addEntry symtbl i (Entry CategoryVariable $ Just t))
    is
    t
symbolTableFuncParam (Left err) _ _ = (Left err)

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
  typeCheck symtbl (Return _) = Right (Nothing, symtbl)
  typeCheck symtbl (Block stmts) =
    case typeCheckList (newFrame symtbl) stmts of
      Right (_, symtbl') -> Right (Nothing, snd $ popFrame symtbl')
      Left err -> Left err
  -- Breaks and continues are trivially well typed
  typeCheck symtbl (Break) = Right (Nothing, symtbl)
  typeCheck symtbl (Continue) = Right (Nothing, symtbl)
  typeCheck symtbl (If ifstmt) = typeCheck symtbl ifstmt
  -- there is no god, also, looks kind of like node.js
  typeCheck symtbl (For maybe_ss maybe_expr maybe_ss' stmts) =
    case typeCheck (newFrame symtbl) maybe_ss of
      Right (_, symtbl') ->
        case typeCheckElemOf symtbl' maybe_expr [(Alias "bool")] of
          Right (_, symtbl'') ->
            case typeCheck symtbl'' maybe_ss' of
              Right (_, symtbl''') ->
                case typeCheckListNewFrame symtbl''' stmts of
                  Right (_, symtbl'''') -> Right (Nothing, symtbl)
                  Left err -> Left err
              Left err -> Left err
          Left err -> Left err
      Left err -> Left err
  typeCheck symtbl (Infinite stmts) = typeCheckListNewFrame symtbl stmts
  typeCheck symtbl (While expr stmts) =
    case typeCheckElemOf symtbl expr [(Alias "bool")] of
      Right (_, symtbl') -> typeCheckListNewFrame symtbl' stmts
      Left err -> Left err
  typeCheck symtbl (Switch maybe_ss maybe_expr clauses) =
    case typeCheck (newFrame symtbl) maybe_ss of
      Right (_, symtbl') ->
        case typeCheck symtbl' maybe_expr of
          Right (Nothing, symtbl'') ->
            case typeCheckClauses symtbl'' clauses (Alias "bool") of
              Right (_, symtbl''') -> Right (Nothing, symtbl)
              Left err -> Left err
          Right (Just t, symtbl'') ->
            case typeCheckClauses symtbl'' clauses t of
              Right (_, symtbl''') -> Right (Nothing, symtbl)
              Left err -> Left err
          Left err -> Left err
      Left err -> Left err

typeCheckClauses
  :: SymbolTable
  -> [Clause]
  -> Type
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
typeCheckClauses symtbl [] _ = Right (Nothing, symtbl)
typeCheckClauses symtbl (Default stmts:xs) t =
  case typeCheckListNewFrame symtbl stmts of
    Right (_, symtbl') -> typeCheckClauses symtbl' xs t
    Left err -> Left err
typeCheckClauses symtbl (Case exprs stmts:xs) t =
  case typeCheckListElemOf symtbl exprs [t] of
    Right (_, symtbl') ->
      case typeCheckListNewFrame symtbl' stmts of
        Right (_, symtbl'') -> typeCheckClauses symtbl'' xs t
        Left err -> Left err
    Left err -> Left err

instance TypeCheckable IfStmt where
  typeCheck symtbl (IfStmt EmptyStmt expr stmts fac) =
    case typeCheckElemOf symtbl expr [(Alias "bool")] of
      Right (Nothing, symtbl') ->
        case typeCheckListNewFrame symtbl' stmts of
          Right (_, symtbl'') -> typeCheck symtbl'' fac
          Left err -> Left err
      Left err -> Left err
  typeCheck symtbl (IfStmt stmt expr stmts fac) =
    case typeCheck (newFrame symtbl) stmt of
      Right (_, symtbl') ->
        case typeCheckElemOf symtbl' expr [(Alias "bool")] of
          Right (_, symtbl'') ->
            case typeCheckListNewFrame symtbl'' stmts of
              Right (_, symtbl''') ->
                case typeCheck symtbl''' fac of
                  Right (_, symtbl'''') -> Right (Nothing, symtbl)
                  Left err -> Left err
              Left err -> Left err
          Left err -> Left err
      Left err -> Left err

instance TypeCheckable IfStmtCont where
  typeCheck symtbl (IfStmtCont Nothing) = Right (Nothing, symtbl)
  typeCheck symtbl (IfStmtCont (Just (Right stmts))) =
    case typeCheckListNewFrame symtbl stmts of
      Right (_, symtbl') -> Right (Nothing, symtbl')
      Left err -> Left err
  typeCheck symtbl (IfStmtCont (Just (Left ifstmt))) = typeCheck symtbl ifstmt

instance TypeCheckable TypeName where
  typeCheck symtbl (TypeName (Alias typename) typeval) =
    case addEntry symtbl (IdOrType typename) (Entry CategoryType $ Just typeval) of
      Right symtbl' -> Right (Nothing, symtbl')
      Left err -> Left (SymbolTableError err, symtbl)

instance TypeCheckable Identifier where
  typeCheck symtbl i =
    case lookupIdentifier symtbl i of
      Right (Entry _ t) -> Right (t, symtbl)
      Left err -> Left (SymbolTableError err, symtbl)

instance TypeCheckable Variable where
  typeCheck symtbl (Variable [] maybe_type []) = Right (Nothing, symtbl)
  typeCheck symtbl (Variable (i:is) maybe_type (e:es)) =
    case addEntry symtbl i (Entry CategoryVariable maybe_type) of
      Right symtbl' -> typeCheck symtbl' (Variable is maybe_type es)
      Left err -> Left (SymbolTableError err, symtbl)

--
instance TypeCheckable SimpleStmt where
  typeCheck symtbl (StmtFuncCall funcCall) = typeCheck symtbl funcCall
  typeCheck symtbl (Incr iden) =
    case typeCheck symtbl iden of
      Right (Just t, symtbl) ->
        if canIncrDecr t
          then Right (Nothing, symtbl)
          else Left
                 ( TypeNotElementOfError
                     (Just t)
                     [(Alias "int"), (Alias "float64")]
                 , symtbl)
  typeCheck symtbl (Decr iden) = typeCheck symtbl (Incr iden)

  -- typeCheck symtbl (Assign idens exprs) =
  -- typeCheck symtbl (ShortBinary op iden expr) =
  -- typeCheck symtbl (ShortVarDec idens exprs) =

-- there must be a better way...
canIncrDecr :: Type -> Bool
canIncrDecr (Alias "int") = True
canIncrDecr (Alias "float64") = True
canIncrDecr _ = False

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

instance TypeCheckable a =>
         TypeCheckable (Maybe a) where
  typeCheck symtbl Nothing = Right (Nothing, symtbl)
  typeCheck symtbl (Just x) = typeCheck symtbl x

instance TypeCheckable Literal where
  typeCheck symtbl (Int' _) = Right (Just (Alias "int"), symtbl)
  typeCheck symtbl (Float64 _) = Right (Just (Alias "float64"), symtbl)
  typeCheck symtbl (Rune _) = Right (Just (Alias "rune"), symtbl)
  typeCheck symtbl (String _) = Right (Just (Alias "string"), symtbl)
  typeCheck symtbl (Raw s) = Left (TypeCheckRawError s, symtbl)
