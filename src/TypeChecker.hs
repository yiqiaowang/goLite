module TypeChecker
  ( TypeCheckError(..)
  , typeCheck
  , typeCheckList
  ) where

import Data.Map.Strict (keys)
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
  | TypeNotCompatableError { typeLeft :: Maybe Type
                          ,  typeRight :: Maybe Type}
  | NoNewIdentifierError
  | AppendNotSliceError
  | StructsNotSameError
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
      Right (_, symtbl') ->
        case popFrame symtbl' of
          Right (f, ps) -> Right (Nothing, ps)
          Left err -> Left (SymbolTableError err, symtbl')
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
    case addEntry
           symtbl
           (IdOrType iden)
           (Entry CategoryVariable $
            Just (Func (functionParamHelper params) ret)) of
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
              Nothing ->
                case popFrame symtbl'' of
                  Right (f, ps) -> Right (Nothing, ps)
                  Left err -> Left (SymbolTableError err, symtbl'')
              Just err -> Left (err, symtbl'')
          Left err -> Left err
      Left err -> Left (SymbolTableError err, symtbl)

-- Function that takes in a list of parameters and returns a list of types
functionParamHelper :: [Parameter] -> [Type]
functionParamHelper [] = []
functionParamHelper (Parameter ids t:ps) =
  [t | _ <- ids] ++ functionParamHelper ps

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
        Nothing ->
          case popFrame symtbl' of
            Right (f, ps) -> typeCheckReturns ps xs ret
            Left err -> Just (SymbolTableError err)
        Just err -> Just err
    Left (err, _) -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Left ifstmt)))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Right (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          case popFrame symtbl' of
            Right (f, ps) -> typeCheckReturns ps ((If ifstmt) : xs) ret
            Left err -> Just (SymbolTableError err)
        Just err -> Just err
    Left (err, _) -> Just err
typeCheckReturns symtbl (If (IfStmt _ _ stmts (IfStmtCont (Just (Right stmts')))):xs) ret =
  case typeCheckList (newFrame symtbl) stmts of
    Right (_, symtbl') ->
      case typeCheckReturns symtbl' stmts ret of
        Nothing ->
          case popFrame symtbl' of
            Right (f, ps) ->
              case typeCheckList (newFrame $ ps) stmts' of
                Right (_, symtbl'') -> Nothing
                Left (err, _) -> Just err
            Left err -> Just (SymbolTableError err)
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
  typeCheck symtbl (StmtDec dec) = typeCheck symtbl dec
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
      Right (_, symtbl') ->
        case popFrame symtbl' of
          Right (f, ps) -> Right (Nothing, ps)
          Left err -> Left (SymbolTableError err, symtbl')
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
  typeCheck symtbl (Switch ss maybe_expr clauses) =
    case typeCheck (newFrame symtbl) ss of
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
  typeCheck symtbl (IfStmt EmptyStmt expr stmts fac) = do
    (_, symtbl') <- typeCheckElemOf symtbl expr [(Alias "bool")]
    (_, symtbl'') <- typeCheckListNewFrame symtbl' stmts
    typeCheck symtbl'' fac
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
  typeCheck symtbl (Variable [] _ []) = Right (Nothing, symtbl)
  typeCheck symtbl (Variable (i:is) maybe_type []) =
    case maybe_type of
      Nothing ->
        case addEntry symtbl i (Entry CategoryVariable maybe_type) of
          Right symtbl' -> typeCheck symtbl' (Variable is maybe_type [])
          Left err -> Left (SymbolTableError err, symtbl)
      Just t ->
        case typeCheck symtbl t of
          Right (t, symtbl') ->
            case addEntry symtbl i (Entry CategoryVariable t) of
              Right symtbl' -> typeCheck symtbl' (Variable is maybe_type [])
              Left err -> Left (SymbolTableError err, symtbl)
          Left err -> Left err
  typeCheck symtbl (Variable (i:is) maybe_type (e:es)) =
    case maybe_type of
      (Just _) ->
        case typeCheck symtbl e of
          Right (t, symtbl') ->
            case typeCheck symtbl' maybe_type of
              Right (t', symtbl'') ->
                case assertTypeEqual t t' of
                  True ->
                    case addEntry symtbl' i (Entry CategoryVariable t') of
                      Right symtbl'' ->
                        typeCheck symtbl'' (Variable is maybe_type es)
                      Left err -> Left (SymbolTableError err, symtbl')
                  False -> Left (TypeMismatchError t' t, symtbl')
              Left err -> Left err
          Left err -> Left err
      Nothing ->
        case typeCheck symtbl e of
          Right (t, symtbl') ->
            case addEntry symtbl' i (Entry CategoryVariable t) of
              Right symtbl'' -> typeCheck symtbl'' (Variable is maybe_type es)
              Left err -> Left (SymbolTableError err, symtbl')
          Left err -> Left err

--
instance TypeCheckable Type where
  typeCheck symtbl (Alias s) =
    case lookupIdentifier symtbl (IdOrType s) of
      Right e ->
        if dataType e `elem`
           [ Just (Alias "int")
           , Just (Alias "float64")
           , Just (Alias "bool")
           , Just (Alias "string")
           , Just (Alias "rune")
           ]
          then Right (dataType e, symtbl)
          else typeCheck symtbl $ dataType e
      Left err -> Left (SymbolTableError err, symtbl)
  typeCheck symtbl (Array t i) =
    case typeCheck symtbl t of
      Right (Just t', symtbl') -> Right (Just (Array t' i), symtbl')
      Right (Nothing, symtbl') ->
        Left (TypeMismatchError (Just t) Nothing, symtbl')
      Left err -> Left err
  typeCheck symtbl (Slice t) =
    case typeCheck symtbl t of
      Right (Just t', symtbl') -> Right (Just (Slice t'), symtbl')
      Right (Nothing, symtbl') ->
        Left (TypeMismatchError (Just t) Nothing, symtbl')
      Left err -> Left err
  -- typeCheck symtbl (Struct) =
  -- typeCheck symtbl (Func a r) =
  -- typeCheck symtbl (Bool a) =

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
  typeCheck symtbl (Assign idens exprs) =
    case typeCheckAssignList symtbl idens exprs of
      Nothing -> Right (Nothing, symtbl)
      Just err -> Left (err, symtbl)
  -- typeCheck symtbl (ShortBinary op iden expr) =
  typeCheck symtbl (ShortVarDec idens exprs) =
    case typeCheckList symtbl exprs of
      Right (_, symtbl') ->
        case svdIdenHelper symtbl' idens of
          Right True ->
            case svdAssignHelper symtbl' idens exprs of
              Nothing -> Right (Nothing, symtbl')
              Just err -> Left (err, symtbl')
          Right False -> Left (NoNewIdentifierError, symtbl')
          Left err -> Left (err, symtbl')
      Left err -> Left err

-- Function that checks that at least one identifier is not declared in the current scope
svdIdenHelper :: SymbolTable -> [Identifier] -> Either TypeCheckError Bool
svdIdenHelper symtbl idens =
  case popFrame symtbl of
    Right (f, ps) -> Right (False `elem` fmap (hasKey $ getMap f) idens)
    Left err -> Left (SymbolTableError err)

svdAssignHelper :: SymbolTable
                -> [Identifier]
                -> [Expression]
                -> Maybe TypeCheckError
svdAssignHelper _ [] [] = Nothing
svdAssignHelper symtbl (iden:idens) (expr:exprs) =
  case popFrame symtbl of
    Right (f, ps) ->
      case hasKey (getMap f) iden of
        False -> svdAssignHelper symtbl idens exprs
        True ->
          case typeCheck symtbl iden of
            Right (t, symtbl') ->
              case typeCheck symtbl' expr of
                Right (t', symtbl'') ->
                  case assertTypeEqual t t' of
                    True -> svdAssignHelper symtbl'' idens exprs
                    False -> Just (TypeMismatchError t t')
                Left (err, _) -> Just err
            Left (err, _) -> Just err
    Left err -> Just (SymbolTableError err)

typeCheckAssignList :: SymbolTable
                    -> [Identifier]
                    -> [Expression]
                    -> Maybe TypeCheckError
typeCheckAssignList _ [] [] = Nothing
typeCheckAssignList symtbl (iden:idens) (expr:exprs) =
  case typeCheckAssign symtbl iden expr of
    Nothing -> typeCheckAssignList symtbl idens exprs
    Just err -> Just err

typeCheckAssign :: SymbolTable
                -> Identifier
                -> Expression
                -> Maybe TypeCheckError
typeCheckAssign symtbl i e =
  case typeCheck symtbl i of
    Right (a, _) ->
      case typeCheck symtbl e of
        Right (b, _) ->
          if assertTypeEqual a b
            then Nothing
            else Just (TypeMismatchError a b)
        Left (err, _) -> Just err
    Left (err, _) -> Just err

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
  typeCheck symtbl (Append ident expr) =
    case typeCheck symtbl ident of
      Right (Just (Slice t), symtbl') ->
        case typeCheck symtbl' expr of
          Right (Just t2, symtbl'') ->
            case assertTypeEqual (Just t) (Just t2) of
              True -> Right (Just (Slice t), symtbl'')
              False -> Left (TypeMismatchError (Just t) (Just t2), symtbl'')
          Right (Nothing, symtbl'') ->
            Left (TypeMismatchError (Just t) Nothing, symtbl'')
          Left (err) -> Left (err)
      Right (_, symtbl') -> Left (AppendNotSliceError, symtbl')
      Left (err) -> Left (err)
  typeCheck symtbl (Unary a expr) =
    case typeCheck symtbl expr of
      Right (t, symtbl') -> unaryCheck (unaryList a) t symtbl'
      Left (err) -> Left (err)
  typeCheck symtbl (Binary a expr1 expr2) =
    case typeCheck symtbl expr1 of
      Right (t, symtbl') ->
        case typeCheck symtbl' expr2 of
          Right (t2, symtbl'') ->
            binaryCheck (binaryList a) (opToCategory a) t t2 symtbl''
          Left (err) -> Left (err)
      Left (err) -> Left (err)

-- Check if a unary expression is correctly typed
unaryList :: UnaryOp -> [Type]
unaryList Pos = [(Alias "int"), (Alias "float64"), (Alias "rune")]
unaryList Neg = [(Alias "int"), (Alias "float64"), (Alias "rune")]
unaryList BoolNot = [(Alias "bool")]
unaryList BitComplement = [(Alias "int"), (Alias "rune")]

unaryCheck
  :: [Type]
  -> (Maybe Type)
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
unaryCheck tList (Just t) symtbl =
  case t `elem` tList of
    True -> Right (Just t, symtbl)
    False -> Left (TypeNotElementOfError (Just t) tList, symtbl)
unaryCheck tList Nothing symtbl =
  Left (TypeNotElementOfError Nothing tList, symtbl)

-- Check if a binary expression is correctly typed
binaryList :: BinaryOp -> [(Type, Type)]
binaryList a =
  case a of
    Or -> [(Alias "bool", Alias "bool")]
    And -> [(Alias "bool", Alias "bool")]
    Equals ->
      [ (Alias "bool", Alias "bool")
      , (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      ]
    NotEquals ->
      [ (Alias "bool", Alias "bool")
      , (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      ]
    LThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    LEThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    GThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    GEThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    Add ->
      [ (Alias "int", Alias "int")
      , (Alias "int", Alias "string")
      , (Alias "string", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    Sub ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    Mult ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    Div ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "int", Alias "float64")
      , (Alias "float64", Alias "int")
      ]
    BitClear -> [(Alias "int", Alias "int")]
    BitRShift -> [(Alias "int", Alias "int")]
    BitLShift -> [(Alias "int", Alias "int")]
    BitXor -> [(Alias "int", Alias "int")]
    BitOr -> [(Alias "int", Alias "int")]
    BitAnd -> [(Alias "int", Alias "int")]

binaryCheck
  :: [(Type, Type)]
  -> ExpressionCategory
  -> (Maybe Type)
  -> (Maybe Type)
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
binaryCheck tList ExpBoolean (Just t1) (Just t2) symtbl =
  case (t1, t2) `elem` tList of
    True -> Right (Just (Alias "bool"), symtbl)
    False -> Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)
binaryCheck tList ExpComparable (Just t1) (Just t2) symtbl =
  case (t1, t2) of
    (Alias s1, Alias s2) ->
      case (Alias s1, Alias s2) `elem` tList of
        True -> Right (Just (Alias "bool"), symtbl)
        False -> Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)
    (Array s1 _, Array s2 _) ->
      binaryCheck tList ExpComparable (Just s1) (Just s2) symtbl
    (Struct s1, Struct s2) -> structListCheck tList ExpComparable s1 s2 symtbl
    (s1, s2) -> Left (TypeNotCompatableError (Just s1) (Just s2), symtbl)
binaryCheck tList ExpOrdered (Just t1) (Just t2) symtbl =
  case (t1, t2) of
    (Alias s1, Alias s2) ->
      case (Alias s1, Alias s2) `elem` tList of
        True -> Right (Just (Alias "bool"), symtbl)
        False -> Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)
    (s1, s2) -> Left (TypeNotCompatableError (Just s1) (Just s2), symtbl)
binaryCheck tList _ (Just t1) (Just t2) symtbl =
  case (t1, t2) of
    (Alias s1, Alias s2) ->
      case (Alias s1, Alias s2) `elem` tList of
        True -> Right (Just (Alias (doubleConvert (s1, s2))), symtbl)
        False -> Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)
    (s1, s2) -> Left (TypeNotCompatableError (Just s1) (Just s2), symtbl)
binaryCheck tList _ t1 t2 symtbl = Left (TypeNotCompatableError t1 t2, symtbl)

-- Type check two structs for comparison
structElementCheck
  :: [(Type, Type)]
  -> ExpressionCategory
  -> ([Identifier], Type)
  -> ([Identifier], Type)
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
structElementCheck tList ExpComparable (_, t1) (_, t2) symtbl =
  case (t1, t2) of
    (Alias s1, Alias s2) ->
      case (Alias s1, Alias s2) `elem` tList of
        True -> Right (Just (Alias "bool"), symtbl)
        False -> Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)
    (Array s1 _, Array s2 _) ->
      binaryCheck tList ExpComparable (Just s1) (Just s2) symtbl
    (Struct s1, Struct s2) -> structListCheck tList ExpComparable s1 s2 symtbl
    (s1, s2) -> Left (TypeNotCompatableError (Just s1) (Just s2), symtbl)
structElementCheck tList _ (_, t1) (_, t2) symtbl =
  Left (TypeNotCompatableError (Just t1) (Just t2), symtbl)

structListCheck
  :: [(Type, Type)]
  -> ExpressionCategory
  -> [([Identifier], Type)]
  -> [([Identifier], Type)]
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
structListCheck tList ExpComparable [] [] symtbl =
  Right (Just (Alias "bool"), symtbl)
structListCheck tList ExpComparable [] _ symtbl =
  Left (StructsNotSameError, symtbl)
structListCheck tList ExpComparable _ [] symtbl =
  Left (StructsNotSameError, symtbl)
structListCheck tList ExpComparable (x1:xs1) (x2:xs2) symtbl =
  case structElementCheck tList ExpComparable x1 x2 symtbl of
    Right (_, symtbl') -> structListCheck tList ExpComparable xs1 xs2 symtbl'
    Left (err) -> Left (err)
structListCheck tList _ _ _ symtbl = Left (StructsNotSameError, symtbl)

-- Different expression categories for types
data ExpressionCategory
  = ExpBoolean
  | ExpComparable
  | ExpOrdered
  | ExpAdd
  | ExpNumeric
  | ExpInteger

-- Convert a binary operation to its expression category
opToCategory :: BinaryOp -> ExpressionCategory
opToCategory a =
  case a of
    Or -> ExpBoolean
    And -> ExpBoolean
    Equals -> ExpComparable
    NotEquals -> ExpComparable
    LThan -> ExpOrdered
    LEThan -> ExpOrdered
    GThan -> ExpOrdered
    GEThan -> ExpOrdered
    Add -> ExpAdd
    Sub -> ExpNumeric
    Div -> ExpNumeric
    Mult -> ExpNumeric
    BitAnd -> ExpInteger
    BitOr -> ExpInteger
    BitXor -> ExpInteger
    BitLShift -> ExpInteger
    BitRShift -> ExpInteger
    BitClear -> ExpInteger

-- Convert a tuple of types to the proper type
doubleConvert :: (String, String) -> String
doubleConvert ("int", "int") = "int"
doubleConvert ("float64", "int") = "int"
doubleConvert ("int", "float64") = "int"
doubleConvert ("float64", "float64") = "float64"
doubleConvert ("int", "string") = "string"
doubleConvert ("string", "string") = "string"
doubleConvert ("string", "int") = "string"
doubleConvert _ = "none"

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
