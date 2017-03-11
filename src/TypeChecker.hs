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
      Right (_, symtbl') -> typeCheckListElemOf symtbl' as types
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
      Right (_, symtbl') -> Right (Nothing, symtbl')
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
typeCheckClauses symtbl (Default stmts:xs) t = do
  (_, symtbl') <- typeCheckListNewFrame symtbl stmts
  typeCheckClauses symtbl' xs t
typeCheckClauses symtbl (Case exprs stmts:xs) t = do
  (_, symtbl') <- typeCheckListElemOf symtbl exprs [t]
  (_, symtbl'') <- typeCheckListNewFrame symtbl' stmts
  typeCheckClauses symtbl'' xs t

--
instance TypeCheckable IfStmt where
  typeCheck symtbl (IfStmt EmptyStmt expr stmts fac) = do
    (_, symtbl') <- typeCheckElemOf symtbl expr [Alias "bool"]
    (_, symtbl'') <- typeCheckListNewFrame symtbl' stmts
    typeCheck symtbl'' fac
  typeCheck symtbl (IfStmt stmt expr stmts fac) = do
    (_, symtbl') <- typeCheck (newFrame symtbl) stmt
    (_, symtbl'') <- typeCheckElemOf symtbl' expr [Alias "bool"]
    (_, symtbl''') <- typeCheckListNewFrame symtbl'' stmts
    (_, symtbl'''') <- typeCheck symtbl''' fac
    Right (Nothing, symtbl')
--     typeCheckIfHelper symtbl' expr stmts fac
--   typeCheck symtbl (IfStmt a@(Assign _ _ ) expr stmts fac) = do
--     (_, symtbl') <- typeCheck (newFrame symtbl) a
--     typeCheckIfHelper symtbl' expr stmts fac
--   typeCheck symtbl _ = Left (InvalidInitStatement, symtbl)
--
-- -- Type checks if statements that contain an init svd/assignment
-- -- statement equivalently
-- typeCheckIfHelper symtbl expr stmts fac = do


--
instance TypeCheckable IfStmtCont where
  typeCheck symtbl (IfStmtCont Nothing) = Right (Nothing, symtbl)
  typeCheck symtbl (IfStmtCont (Just (Right stmts))) = do
    (_, symtbl') <- typeCheckListNewFrame symtbl stmts
    Right (Nothing, symtbl')
  typeCheck symtbl (IfStmtCont (Just (Left ifstmt))) = typeCheck symtbl ifstmt

instance TypeCheckable TypeName where
  typeCheck symtbl (TypeName (Alias typename) typeval) =
    case addEntry
           symtbl
           (IdOrType typename)
           (Entry CategoryAlias $ Just typeval) of
      Right symtbl' -> Right (Nothing, symtbl')
      Left err -> Left (SymbolTableError err, symtbl)

instance TypeCheckable Identifier where
  typeCheck symtbl (IdOrType i) =
    case lookupIdentifier symtbl (IdOrType i) of
      Right (Entry _ t) -> Right (t, symtbl)
      Left err -> Left (SymbolTableError err, symtbl)
  typeCheck symtbl (IdArray i _) =
    case lookupIdentifier symtbl (IdOrType i) of
      Right (Entry CategoryType t) -> Right (t, symtbl)
      Right (Entry CategoryVariable t) -> typeCheck symtbl t
      Left err -> Left (SymbolTableError err, symtbl)
  typeCheck symtbl (IdField is) =
    case structHelper symtbl is of
      Right t -> Right (Just t, symtbl)
      Left err -> Left (err, symtbl)

    -- case lookupIdentifier symtbl (last is) of
    --   Right (Entry CategoryType t) -> Right (t, symtbl)
    --   Right (Entry CategoryVariable t) -> typeCheck symtbl t
    --   Left err -> Left (SymbolTableError err, symtbl)



-- Get struct internals
getStructInternal :: SymbolTable -> Identifier -> Either TypeCheckError [([Identifier], Type)]
getStructInternal symtbl i =
  case lookupIdentifier symtbl i of
    Left err -> Left (SymbolTableError err)
    Right (Entry CategoryVariable (Just (Alias k))) -> getStructInternal symtbl (IdOrType k)
    Right (Entry CategoryType (Just (Struct k))) -> Right k
    Right _ -> Left StructAccessError



-- Struct helper
structHelper :: SymbolTable -> [Identifier] -> Either TypeCheckError Type
structHelper symtbl [i,j] =
  case getStructInternal symtbl i of
    Right k -> structFieldHelper k i
    Left err -> Left err

structHelper symtbl (i:j:js) =
  case getStructInternal symtbl i of
    Right k -> case structFieldHelper k j of
                 Right (Alias t) -> structHelper symtbl ((IdOrType t):js)
    Left err -> Left err
  

-- Helper for type checking struct identifiers
-- structIdHelper :: SymbolTable -> [Identifier] -> Either TypeCheckError Type

-- structIdHelper symtbl [i,j] =
--   case lookupIdentifier symtbl i of
--     Right (Entry CategoryAlias (Just (Struct js))) ->
--       case structFieldHelper js j of
--         Left err -> Left err
--         Right t -> Right t
--     Right (Entry CategoryVariable (Just (Alias k))) ->
--       structIdHelper symtbl [(IdOrType k), j]
--     Left err -> Left (SymbolTableError err)

-- structIdHelper symtbl (i:j:js) =
--   case lookupIdentifier symtbl i of
--     Right (Entry CategoryAlias (Just (Struct k))) ->
--       case structFieldHelper k j of
--         Left err -> Left err
--         Right t -> structIdHelper symtbl (j:js) 
--     Right (Entry CategoryAlias (Just (Alias k))) ->
--       structIdHelper symtbl ((IdOrType k):(j:js))
--     Right (Entry CategoryVariable (Just (Alias k))) ->
--       structIdHelper symtbl ((IdOrType k):(j:js))
--     Left err -> Left (SymbolTableError err)

structFieldHelper :: [([Identifier], Type)]
                  -> Identifier
                  -> Either TypeCheckError Type
structFieldHelper [] _ = Left StructAccessError
structFieldHelper (x:xs) i =
  if i `elem` (fst $ x)
    then Right (snd $ x)
    else structFieldHelper xs i

instance TypeCheckable Variable where
  typeCheck symtbl (Variable [] _ []) = Right (Nothing, symtbl)
  typeCheck symtbl (Variable (i:is) maybe_type []) =
    case maybe_type of
      Nothing ->
        case addEntry symtbl i (Entry CategoryVariable maybe_type) of
          Right symtbl' -> typeCheck symtbl' (Variable is maybe_type [])
          Left err -> Left (SymbolTableError err, symtbl)
      Just t ->
        case varDecHelper symtbl (Just t) of
          Nothing ->
            case addEntry symtbl i (Entry CategoryVariable (Just t)) of
              Right symtbl' -> typeCheck symtbl' (Variable is maybe_type [])
              Left err -> Left (SymbolTableError err, symtbl)
          Just err -> Left (err, symtbl)
  typeCheck symtbl (Variable (i:is) maybe_type (e:es)) =
    case maybe_type of
      (Just _) ->
        case typeCheck symtbl e of
          Right (t, symtbl'')
            -- case typeCheck symtbl' maybe_type of
              -- Right (t', symtbl'') ->
           ->
            case varDecHelper symtbl'' maybe_type of
              Nothing ->
                case assertTypeEqual t maybe_type of
                  True ->
                    case addEntry symtbl'' i (Entry CategoryVariable maybe_type) of
                      Right symtbl''' ->
                        typeCheck symtbl''' (Variable is maybe_type es)
                      Left err -> Left (SymbolTableError err, symtbl'')
                  False -> Left (TypeMismatchError maybe_type t, symtbl'')
              Just err -> Left (err, symtbl'')
              -- Left err -> Left err
          Left err -> Left err
      Nothing ->
        case typeCheck symtbl e of
          Right (t, symtbl') ->
            case addEntry symtbl' i (Entry CategoryVariable t) of
              Right symtbl'' -> typeCheck symtbl'' (Variable is maybe_type es)
              Left err -> Left (SymbolTableError err, symtbl')
          Left err -> Left err

-- Gets checks if the type being assigned to a variable is valid
-- varDecHelper :: SymbolTable -> Maybe Type -> Either TypeCheckError (Maybe Type)
-- varDecHelper symtbl (Just (Alias s)) =
--   case lookupIdentifier symtbl (IdOrType s) of
--     Right e ->
--       case e of
--         Entry _ Nothing -> Left (UndefinedTypeError s)
--         Entry CategoryVariable _ -> Left (VariableAsTypeError s)
--         Entry CategoryAlias t -> Right t
--         Entry CategoryType t -> Right t
--     Left err -> Left (SymbolTableError err)
-- varDecHelper symtbl (Just (Array t _)) = varDecHelper symtbl (Just t)
-- varDecHelper symtbl (Just (Slice t)) = varDecHelper symtbl (Just t)
-- varDecHelper _ _ = Left VariableDecError
varDecHelper :: SymbolTable -> Maybe Type -> Maybe TypeCheckError
varDecHelper symtbl (Just (Alias s)) =
  case lookupIdentifier symtbl (IdOrType s) of
    Right e ->
      case e of
        Entry _ Nothing -> Just (UndefinedTypeError s)
        Entry CategoryVariable _ -> Just (VariableAsTypeError s)
        Entry CategoryAlias t -> Nothing
        Entry CategoryType t -> Nothing
    Left err -> Just (SymbolTableError err)
varDecHelper symtbl (Just (Array t _)) = varDecHelper symtbl (Just t)
varDecHelper symtbl (Just (Slice t)) = varDecHelper symtbl (Just t)
varDecHelper _ _ = Just VariableDecError

--
getBaseType
  :: SymbolTable
  -> Identifier
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
getBaseType symtbl s =
  case lookupIdentifier symtbl s of
    Right (Entry c t) ->
      if (c == CategoryType) || isStruct t
        then Right (t, symtbl)
        else case t of
               Just (Alias s) -> getBaseType symtbl (IdOrType s)
               Just (Array (Alias s) _) -> getBaseType symtbl (IdOrType s)
               Just (Slice (Alias s)) -> getBaseType symtbl (IdOrType s)
    Left err -> Left (SymbolTableError err, symtbl)

isStruct :: Maybe Type -> Bool
isStruct (Just (Struct _)) = True
isStruct _ = False

--
instance TypeCheckable Type
  -- Or should this return typeChek symtbl (IdOrType s)?
                                                         where
  typeCheck symtbl (Alias s) = Right (Just (Alias s), symtbl)
  typeCheck symtbl (Array t i) = typeCheck symtbl t
    -- case typeCheck symtbl t of
    --   Right (Just t', symtbl') -> Right (Just (Array t' i), symtbl')
    --   Right (Nothing, symtbl') ->
    --     Left (TypeMismatchError (Just t) Nothing, symtbl')
    --   Left err -> Left err
  typeCheck symtbl (Slice t) = typeCheck symtbl t
    -- case typeCheck symtbl t of
    --   Right (Just t', symtbl') -> Right (Just (Slice t'), symtbl')
    --   Right (Nothing, symtbl') ->
    --     Left (TypeMismatchError (Just t) Nothing, symtbl')
    --   Left err -> Left err

--
instance TypeCheckable SimpleStmt where
  typeCheck symtbl (StmtFuncCall funcCall) = typeCheck symtbl funcCall
  typeCheck symtbl (EmptyStmt) = Right (Nothing, symtbl)
  -- Need to do getBaseType, and accept all numeric base types,
  -- reject array, slice and struct types unless they are indexed.
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
      Left err -> Left err
  typeCheck symtbl (Decr iden) = typeCheck symtbl (Incr iden)
  typeCheck symtbl (Assign idens exprs) =
    case typeCheckAssignList symtbl idens exprs of
      Nothing -> Right (Nothing, symtbl)
      Just err -> Left (err, symtbl)
  typeCheck symtbl (ShortBinary op iden expr) =
    case op of
      PlusEq -> typeCheck symtbl (Assign [iden] [Binary Add (Id iden) expr])
      MinusEq -> typeCheck symtbl (Assign [iden] [Binary Sub (Id iden) expr])
      MulEq -> typeCheck symtbl (Assign [iden] [Binary Mult (Id iden) expr])
      DivEq -> typeCheck symtbl (Assign [iden] [Binary Div (Id iden) expr])
      ModEq -> typeCheck symtbl (Assign [iden] [Binary Mod (Id iden) expr])
      BitAndEq ->
        typeCheck symtbl (Assign [iden] [Binary BitAnd (Id iden) expr])
      BitOrEq -> typeCheck symtbl (Assign [iden] [Binary BitOr (Id iden) expr])
      BitXorEq ->
        typeCheck symtbl (Assign [iden] [Binary BitXor (Id iden) expr])
      BitLShiftEq ->
        typeCheck symtbl (Assign [iden] [Binary BitLShift (Id iden) expr])
      BitRShiftEq ->
        typeCheck symtbl (Assign [iden] [Binary BitRShift (Id iden) expr])
      BitClearEq ->
        typeCheck symtbl (Assign [iden] [Binary BitClear (Id iden) expr])
  typeCheck symtbl (ShortVarDec idens exprs) =
    case typeCheckList symtbl exprs of
      Right (_, symtbl') ->
        case svdIdenHelper symtbl' idens of
          Right True ->
            case svdAssignHelper symtbl' idens exprs of
              Right symtbl'' -> Right (Nothing, symtbl'')
              Left err -> Left (err, symtbl')
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
                -> Either TypeCheckError SymbolTable
svdAssignHelper symtbl [] [] = Right symtbl
svdAssignHelper symtbl (iden:idens) (expr:exprs) =
  case popFrame symtbl of
    Right (f, ps) ->
      case hasKey (getMap f) iden of
        False ->
          case typeCheck symtbl expr of
            Right (t, symtbl') ->
              case typeCheck symtbl' (Variable [iden] t [expr]) of
                Right (_, symtbl'') -> svdAssignHelper symtbl'' idens exprs
                Left (err, _) -> Left err
        True ->
          case typeCheck symtbl iden of
            Right (t, symtbl') ->
              case typeCheck symtbl' expr of
                Right (t', symtbl'') ->
                  case assertTypeEqual t t' of
                    True -> svdAssignHelper symtbl'' idens exprs
                    False -> Left (TypeMismatchError t t')
                Left (err, _) -> Left err
            Left (err, _) -> Left err
    Left err -> Left (SymbolTableError err)

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
canIncrDecr (Alias "rune") = True
canIncrDecr _ = False

instance TypeCheckable FunctionCall where
  typeCheck symtbl (FunctionCall funcName exprs) =
    case lookupIdentifier symtbl (IdOrType funcName) of
      Right (Entry CategoryType (Just (t))) ->
        if length exprs /= 1
          then Left (EmptyCastError, symtbl)
          else typeCheckCast symtbl t (head exprs)
      Right (Entry CategoryVariable (Just (Func ts ret))) ->
        if length ts == length exprs
          then case functionCallHelper symtbl ts exprs of
                 Nothing -> Right (ret, symtbl)
                 Just err -> Left err
          else Left (FuncCallArgNumError ts exprs, symtbl)
      Right (Entry _ Nothing) -> Left (DefinitionNotFoundError, symtbl)
      Left err -> Left (SymbolTableError err, symtbl)

-- Type Checks a cast. make sure the cast can be made.
typeCheckCast
  :: SymbolTable
  -> Type
  -> Expression
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
typeCheckCast symtbl t e =
  case typeCheck symtbl e of
    Right (Just x, symtbl') ->
      case (t, x) of
        (Alias "int", Alias "int") -> Right (Just (Alias "int"), symtbl')
        (Alias "int", Alias "float64") -> Right (Just (Alias "int"), symtbl')
        (Alias "int", Alias "rune") -> Right (Just (Alias "int"), symtbl')
        (Alias "int", Alias "bool") -> Right (Just (Alias "int"), symtbl')
        (Alias "bool", Alias "bool") -> Right (Just (Alias "bool"), symtbl')
        (Alias "bool", Alias "int") -> Right (Just (Alias "bool"), symtbl')
        (Alias "bool", Alias "float64") -> Right (Just (Alias "bool"), symtbl')
        (Alias "bool", Alias "rune") -> Right (Just (Alias "bool"), symtbl')
        (Alias "float64", Alias "float64") ->
          Right (Just (Alias "float64"), symtbl')
        (Alias "float64", Alias "int") ->
          Right (Just (Alias "float64"), symtbl')
        (Alias "float64", Alias "bool") ->
          Right (Just (Alias "float64"), symtbl')
        (Alias "float64", Alias "rune") ->
          Right (Just (Alias "float64"), symtbl')
        (Alias "rune", Alias "rune") -> Right (Just (Alias "rune"), symtbl')
        (Alias "rune", Alias "int") -> Right (Just (Alias "rune"), symtbl')
        (Alias "rune", Alias "float64") -> Right (Just (Alias "rune"), symtbl')
        (Alias "rune", Alias "bool") -> Right (Just (Alias "rune"), symtbl')
        _ -> Left (IllegalCastError t x, symtbl')
    Left err -> Left err

-- Takes in a list of types, and a list of expressions, and makes sure they match up
-- this also type checks the expressions, as it must
functionCallHelper :: SymbolTable
                   -> [Type]
                   -> [Expression]
                   -> Maybe (TypeCheckError, SymbolTable)
functionCallHelper _ [] [] = Nothing
functionCallHelper symtbl (t:ts) (e:es) =
  case typeCheck symtbl e of
    Right (x, symtbl') ->
      if assertTypeEqual (Just t) x
        then functionCallHelper symtbl' ts es
        else Just (FuncCallArgTypeError t x, symtbl')
    Left err -> Just err

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
            case assertTypeEqual t t2 of
              True -> binaryCheck (binaryList a) (opToCategory a) t symtbl''
              False -> Left (TypeMismatchError t t2, symtbl'')
          Left (err) -> Left (err)
      Left (err) -> Left (err)
  typeCheck symtbl (ExprFuncCall funcCall) = typeCheck symtbl funcCall

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
  case t of
    (Alias str) ->
      case getBaseType symtbl (IdOrType str) of
        Right ((Just t'), symtbl') ->
          case t' `elem` tList of
            True -> Right (Just t, symtbl')
            False -> Left (TypeNotElementOfError (Just t) tList, symtbl')
        Left (err) -> Left (err)
    _ -> Left (TypeNotElementOfError (Just t) tList, symtbl)
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
      ]
    LEThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      ]
    GThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      ]
    GEThan ->
      [ (Alias "rune", Alias "rune")
      , (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      ]
    Add ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "string", Alias "string")
      , (Alias "rune", Alias "rune")
      ]
    Sub ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "rune", Alias "rune")
      ]
    Mult ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "rune", Alias "rune")
      ]
    Div ->
      [ (Alias "int", Alias "int")
      , (Alias "float64", Alias "float64")
      , (Alias "rune", Alias "rune")
      ]
    Mod -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitClear -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitRShift -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitLShift -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitXor -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitOr -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]
    BitAnd -> [(Alias "int", Alias "int"), (Alias "rune", Alias "rune")]

binaryCheck
  :: [(Type, Type)]
  -> ExpressionCategory
  -> (Maybe Type)
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
binaryCheck tList ExpBoolean (Just (Alias str1)) symtbl =
  case getBaseType symtbl (IdOrType str1) of
    Right (Just t1', symtbl1) ->
      case (t1', t1') `elem` tList of
        True -> Right (Just (Alias "bool"), symtbl1)
        False -> Left (InvalidTypeForOpError (Just (Alias str1)), symtbl1)
    _ -> Left (DefinitionNotFoundError, symtbl)
binaryCheck tList ExpComparable (Just t1) symtbl =
  case t1 of
    (Alias s1) ->
      case getBaseType symtbl (IdOrType s1) of
        (Right (Just (Alias s1), symtbl1)) ->
          case (Alias s1, Alias s1) `elem` tList of
            True -> Right (Just (Alias "bool"), symtbl1)
            False -> Left (InvalidTypeForOpError (Just t1), symtbl1)
        (Right (Just (Struct _), symtbl')) ->
          Right (Just (Alias "bool"), symtbl')
        Left err -> Left err
    (Array s1 _) -> comparableCheck s1 symtbl
    (Struct s1) -> structListCheck s1 symtbl
    _ -> Left (DefinitionNotFoundError, symtbl)
binaryCheck tList ExpOrdered (Just t1) symtbl =
  case t1 of
    (Alias s1) ->
      case getBaseType symtbl (IdOrType s1) of
        (Right (Just (Alias s1), symtbl1)) ->
          case (Alias s1, Alias s1) `elem` tList of
            True -> Right (Just (Alias "bool"), symtbl1)
            False -> Left (InvalidTypeForOpError (Just t1), symtbl1)
    _ -> Left (DefinitionNotFoundError, symtbl)
binaryCheck tList _ (Just t1) symtbl =
  case t1 of
    (Alias s1) ->
      case getBaseType symtbl (IdOrType s1) of
        (Right (Just (Alias s1), symtbl1)) ->
          case (Alias s1, Alias s1) `elem` tList of
            True -> Right (Just (Alias s1), symtbl1)
            False -> Left (InvalidTypeForOpError (Just t1), symtbl1)
    _ -> Left (DefinitionNotFoundError, symtbl)
binaryCheck tList _ t1 symtbl = Left (InvalidTypeForOpError t1, symtbl)

-- Type check two structs for comparison
comparableCheck
  :: Type
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
comparableCheck t1 symtbl =
  case t1 of
    (Alias s1) ->
      case (getBaseType symtbl (IdOrType s1)) of
        Right (baseT, symtbl') ->
          case (Alias s1, Alias s1) `elem` (binaryList Equals) of
            True -> Right (Just (Alias "bool"), symtbl')
            False -> Left (ElementsNotComparableError, symtbl')
        Left (err) -> Left (err)
    (Array s1 _) -> comparableCheck s1 symtbl
    (Struct s1) -> structListCheck s1 symtbl
    s1 -> Left (ElementsNotComparableError, symtbl)

structListCheck
  :: [([Identifier], Type)]
  -> SymbolTable
  -> Either (TypeCheckError, SymbolTable) (Maybe Type, SymbolTable)
structListCheck [] symtbl = Right (Just (Alias "bool"), symtbl)
structListCheck ((_, t):xs) symtbl =
  case comparableCheck t symtbl of
    Right (_, symtbl') -> structListCheck xs symtbl'
    Left (err) -> Left (err)

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
    Mod -> ExpInteger
    BitAnd -> ExpInteger
    BitOr -> ExpInteger
    BitXor -> ExpInteger
    BitLShift -> ExpInteger
    BitRShift -> ExpInteger
    BitClear -> ExpInteger

instance TypeCheckable a =>
         TypeCheckable (Maybe a) where
  typeCheck symtbl Nothing = Right (Nothing, symtbl)
  typeCheck symtbl (Just x) = typeCheck symtbl x

instance TypeCheckable Literal where
  typeCheck symtbl (Int' _) = Right (Just (Alias "int"), symtbl)
  typeCheck symtbl (Float64 _) = Right (Just (Alias "float64"), symtbl)
  typeCheck symtbl (Rune _) = Right (Just (Alias "rune"), symtbl)
  typeCheck symtbl (String _) = Right (Just (Alias "string"), symtbl)
  typeCheck symtbl (Raw _) = Right (Just (Alias "string"), symtbl)
