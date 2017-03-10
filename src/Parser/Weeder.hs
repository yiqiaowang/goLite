module Parser.Weeder
  ( WeederError(..)
  , weed
  ) where

import Data.List (group)
import Language.Language

--
data WeederError
  = MultipleDefaultsInSwitch { numDefaults :: Int}
  | AssignmentCountMismatch { numExpected :: Int
                           ,  numFound :: Int}
  | RepeatedIdentifierInSVD { indentifier :: Identifier}
  | InvalidSVD
  | InvalidBreak
  | InvalidContinue
  | InvalidReturn
  | MissingReturn
  | InvalidPostStatement { statement :: SimpleStmt}
  | InvalidPackageName { name :: String}
  | InvalidIdentifier
  | InvalidSVDIdentifier
  deriving (Eq, Show)

--
data Context
  = CFunction
  | CLoop
  deriving (Eq)

--
class Weedable a where
  weed :: a -> Maybe [WeederError]
  weed = weedCtxt []
  weedCtxt :: [Context] -> a -> Maybe [WeederError]
  weedCtxt ctxt = weed
  weedListCtxt :: [Context] -> [a] -> Maybe [WeederError]
  weedListCtxt ctxt ws = mconcat $ map (weedCtxt ctxt) ws

--
instance Weedable Program where
  weed (Program package alls) =
    weedListCtxt [] alls `mappend` weedPackage package
    where
      weedPackage "_" = Just [InvalidPackageName "_"]
      weedPackage _ = Nothing

--
instance Weedable All where
  weed (TopDec stmt) = weed stmt
  weed (Function "main" _ _ stmts) = weedListCtxt [CFunction] stmts
  weed (Function _ _ Nothing stmts) = weedListCtxt [CFunction] stmts
  weed (Function _ _ (Just _) stmts) =
    weedListCtxt [CFunction] stmts `mappend` (isTerminating $ last stmts)

instance Weedable TopLevel where
  weed (VarDec var) = weed var
  weed (VarDecList vars) = weedListCtxt [] vars
  weed _ = Nothing


-- A function to assert whether a given statement, which should be terminating,
-- is terminating or not. If it is, Nothing is returned, otherwise, a MissingReturn
-- Error is returned.
isTerminating :: Stmt -> Maybe [WeederError]
isTerminating (Return _) = Nothing
isTerminating (If (IfStmt _ _ xs (IfStmtCont (Just (Left a))))) =
  isTerminatingList xs `mappend` isTerminating (If a)
isTerminating (If (IfStmt _ _ xs (IfStmtCont (Just (Right a))))) =
  isTerminatingList xs `mappend` isTerminatingList a
isTerminating (For _ Nothing _ xs) =
  if hasBreak xs
    then Just [MissingReturn]
    else Nothing
isTerminating (Infinite xs) =
  if hasBreak xs
    then Just [MissingReturn]
    else Nothing
isTerminating (Switch _ _ cs) =
  if not $ hasDefault cs
    then Just [MissingReturn]
    else isTerminatingClauseList cs -- isTerminating (StmtBlock (Block xs)) = isTerminatingList xs
isTerminating (Block xs) = isTerminatingList xs
isTerminating _ = Just [MissingReturn]

-- Wrapper to assert that the Last statement in a statement list is terminating.
-- The empty Stmt list is trivially non-terminating
isTerminatingList :: [Stmt] -> Maybe [WeederError]
isTerminatingList [] = Just [MissingReturn]
isTerminatingList xs = isTerminating $ last xs

-- A clause is terminating if its statement list is terminating.
isTerminatingClause :: Clause -> Maybe [WeederError]
isTerminatingClause (Case _ xs) =
  if hasBreak xs
    then Just [MissingReturn]
    else isTerminatingList xs
isTerminatingClause (Default xs) =
  if hasBreak xs
    then Just [MissingReturn]
    else isTerminatingList xs

-- Not to be confused by the naming similar to isTerminatingList,
-- isTerminatingClauseList doesnt asserts that the last Clause is terminating.
-- Actually, it asserts that ALL clauses in the clause list are terminating.
isTerminatingClauseList :: [Clause] -> Maybe [WeederError]
isTerminatingClauseList [] = Just [MissingReturn]
isTerminatingClauseList xs = mconcat $ map isTerminatingClause xs

-- Asserts that a statement list contains a Break Statement
hasBreak :: [Stmt] -> Bool
hasBreak [] = False
hasBreak (Break:xs) = True
hasBreak (_:xs) = hasBreak xs

-- Asserts that a clause list contains a default clause
hasDefault :: [Clause] -> Bool
hasDefault [] = False
hasDefault (Default _:xs) = True
hasDefault (_:xs) = hasDefault xs

--
instance Weedable Stmt where
  weedCtxt ctxt (If i) = weedCtxt ctxt i
  weedCtxt ctxt (Switch _ maybe_expr clauses) =
    weedListCtxt ctxt' clauses `mappend`
    let n = length $ filter isDefault clauses
    in if n > 1
         then Just [MultipleDefaultsInSwitch n]
         else case maybe_expr of
                Nothing -> Nothing
                (Just expr) -> weedBlankIdentifier expr
    where
      isDefault (Case _ _) = False
      isDefault (Default _) = True
      ctxt' = CLoop:ctxt
  weedCtxt ctxt (Infinite stmts) = weedListCtxt (CLoop : ctxt) stmts
  weedCtxt ctxt (While _ stmts) = weedCtxt ctxt (Infinite stmts)
  weedCtxt ctxt (For _ maybe_expr post stmts) =
    weedCtxt ctxt (Infinite stmts) `mappend` weedPost post `mappend`
    case maybe_expr of
      Nothing -> Nothing
      (Just expr) -> weedBlankIdentifier expr
    where
      weedPost s@(ShortVarDec _ _) = Just [InvalidPostStatement s]
      weedPost e@(StmtFuncCall _) = Just [InvalidPostStatement e]
      weedPost _ = Nothing
  weedCtxt ctxt (SimpleStmt stmt) = weedCtxt ctxt stmt `mappend` weed stmt
  weedCtxt ctxt Break =
    if CLoop `elem` ctxt
      then Nothing
      else Just [InvalidBreak]
  weedCtxt ctxt Continue =
    if CLoop `elem` ctxt
      then Nothing
      else Just [InvalidContinue]
  weedCtxt ctxt (Return maybe_expr) =
    if CFunction `elem` ctxt
      then case maybe_expr of
             Nothing -> Nothing
             (Just expr) -> weedBlankIdentifier expr
      else Just [InvalidReturn]
  weedCtxt _ (Print exprs) = weedBlankIdentifiers exprs
  weedCtxt _ (StmtDec topLevel) = weed topLevel
  weedCtxt _ _ = Nothing

--
instance Weedable SimpleStmt
  -- This may need to be changed
                                 where
  weedCtxt ctxt (ShortVarDec _ _) =
    if CFunction `elem` ctxt
      then Nothing
      else Just [InvalidSVD]
  weedCtxt ctxt _ = Nothing
  --
  weed (ShortVarDec ids exps) =
    weedAssignmentLength ids exps `mappend` weedSVDIdenListType ids `mappend`
    (let repeatedIds = map head $ filter (\ids' -> length ids' > 1) $ group ids
     in if null repeatedIds
          then Nothing
          else Just $ map RepeatedIdentifierInSVD repeatedIds) `mappend`
    weedBlankIdentifiers exps
  weed (Assign ids exps) =
    weedAssignmentLength ids exps `mappend` weedBlankIdentifiers exps
  weed (Incr (IdOrType "_")) = Just [InvalidIdentifier]
  weed (Decr (IdOrType "_")) = Just [InvalidIdentifier]
  weed (ShortBinary _ iden exps) =
    weedBlankIdentifiers' [iden] `mappend` weedBlankIdentifier exps
  weed (StmtFuncCall funcCall) = weed funcCall
  weed _ = Nothing

instance Weedable FunctionCall where
  weed (FunctionCall iden exprs) = weedBlankIdentifiers exprs

weedSVDIdenType :: Identifier -> Maybe [WeederError]
weedSVDIdenType iden =
  case iden of
    (IdOrType _) -> Nothing
    (IdArray _ _) -> Just [InvalidSVDIdentifier]
    (IdField _) -> Just [InvalidSVDIdentifier]

weedSVDIdenListType :: [Identifier] -> Maybe [WeederError]
weedSVDIdenListType [] = Nothing
weedSVDIdenListType (x:xs) = weedSVDIdenType x `mappend` weedSVDIdenListType xs

--
instance Weedable Variable where
  weed (Variable _ _ []) = Nothing
  weed (Variable ids _ exps) =
    weedAssignmentLength ids exps `mappend` weedBlankIdentifiers exps

--
instance Weedable IfStmt where
  weedCtxt ctxt (IfStmt _ expr stmts (IfStmtCont Nothing)) =
    weedListCtxt ctxt stmts `mappend` weedBlankIdentifier expr
  weedCtxt ctxt (IfStmt _ expr stmts (IfStmtCont (Just (Left i)))) =
    weedListCtxt ctxt stmts `mappend` weedCtxt ctxt i `mappend`
    weedBlankIdentifier expr
  weedCtxt ctxt (IfStmt _ expr stmts (IfStmtCont (Just (Right stmts')))) =
    weedListCtxt ctxt stmts `mappend` weedListCtxt ctxt stmts' `mappend`
    weedBlankIdentifier expr

--
instance Weedable Clause where
  weedCtxt ctxt (Case _ stmts) = weedListCtxt ctxt stmts
  weedCtxt ctxt (Default stmts) = weedListCtxt ctxt stmts

-- Verifies that the number of identifiers is equal to the number of exp
-- in any type of assignment
weedAssignmentLength :: [Identifier] -> [Expression] -> Maybe [WeederError]
weedAssignmentLength ids exps =
  let (numIds, numExps) = (length ids, length exps)
  in if numIds == numExps
       then Nothing
       else Just [AssignmentCountMismatch numIds numExps]

-- Verifies that no blank identifiers are used in expressions
weedBlankIdentifiers :: [Expression] -> Maybe [WeederError]
weedBlankIdentifiers [] = Nothing
weedBlankIdentifiers (x:xs) =
  weedBlankIdentifier x `mappend` weedBlankIdentifiers xs

weedBlankIdentifier :: Expression -> Maybe [WeederError]
weedBlankIdentifier (Brack x) = weedBlankIdentifier x
weedBlankIdentifier (Id (IdOrType "_")) = Just [InvalidIdentifier]
weedBlankIdentifier (Id (IdArray "_" _)) = Just [InvalidIdentifier]
weedBlankIdentifier (Id (IdArray _ exprs)) = weedBlankIdentifiers exprs
weedBlankIdentifier (Id (IdField ids)) = weedBlankIdentifiers' ids
weedBlankIdentifier (Binary _ e1 e2) =
  weedBlankIdentifier e1 `mappend` weedBlankIdentifier e2
weedBlankIdentifier (Unary _ e1) = weedBlankIdentifier e1
weedBlankIdentifier (Append (IdOrType "_") _) = Just [InvalidIdentifier]
weedBlankIdentifier (Append _ e1) = weedBlankIdentifier e1
weedBlankIdentifier (ExprFuncCall (FunctionCall  "_" _)) =
  Just [InvalidIdentifier]
weedBlankIdentifier (ExprFuncCall (FunctionCall _ e1)) = weedBlankIdentifiers e1
weedBlankIdentifier _ = Nothing

weedBlankIdentifiers' :: [Identifier] -> Maybe [WeederError]
weedBlankIdentifiers' [] = Nothing
weedBlankIdentifiers' (i:is) =
  weedBlankIdentifier (Id i) `mappend` weedBlankIdentifiers' is
