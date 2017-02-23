module Weeder
  ( WeederError(..)
  , weed
  ) where


import Data.List(group)
import Language


--
data WeederError
  = MultipleDefaultsInSwitch
    { numDefaults :: Int }
  | AssignmentCountMismatch
    { numExpected :: Int
    , numFound :: Int
    }
  | RepeatedIdentifierInSVD
    { indentifier :: String }
  | InvalidBreak
  | InvalidContinue
  | InvalidReturn
  | MissingReturn
  | InvalidPostStatement
    { statement :: SimpleStmt }
  deriving (Eq, Show)


--
data Context
  = CFunction
  | CLoop
  deriving Eq


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
  weed (Program package alls) = weedListCtxt [] alls


--
instance Weedable All where
  weed (Stmt stmt) = weed stmt
  weed (Function "main" _ _ stmts) =
    weedListCtxt [] stmts
  weed (Function _ _ _ stmts) =
    weedListCtxt [CFunction] stmts `mappend`
      if any isReturn stmts
        then Nothing
        else Just [MissingReturn]
      where
        isReturn (Return _) = True
        isReturn _ = False


--
instance Weedable Stmt where
  weedCtxt ctxt (If i) = weedCtxt ctxt i
  weedCtxt ctxt (Switch _ _ clauses) =
    weedListCtxt ctxt clauses `mappend`
      let n = length $ filter isDefault clauses in
        if n > 1
          then Just [MultipleDefaultsInSwitch n]
          else Nothing
      where
        isDefault (Case _ _) = False
        isDefault (Default _) = True
  weedCtxt ctxt (Infinite stmts) = weedListCtxt (CLoop : ctxt) stmts
  weedCtxt ctxt (While _ stmts) = weedCtxt ctxt (Infinite stmts)
  weedCtxt ctxt (For _ _ (Just post) stmts) =
    weedCtxt ctxt (Infinite stmts) `mappend` weedPost post
    where
      weedPost s@(ShortVarDec _ _) = Just [InvalidPostStatement s]
      weedPost e@(ExprStmt _) = Just [InvalidPostStatement e]
      weedPost _ = Nothing
  weedCtxt ctxt (SimpleStmt stmt) = weedCtxt ctxt stmt
  weedCtxt ctxt Break =
    if CLoop `elem` ctxt
      then Nothing
      else Just [InvalidBreak]
  weedCtxt ctxt Continue =
    if CLoop `elem` ctxt
      then Nothing
      else Just [InvalidContinue]
  weedCtxt ctxt (Return _) =
    if CFunction `elem` ctxt
      then Nothing
      else Just [InvalidReturn]
  weedCtxt _  (VarDec var) = weed var
  weedCtxt _ _ = Nothing


--
instance Weedable SimpleStmt where
  weed (ShortVarDec ids exps) =
    weedAssignmentLength ids exps `mappend`
      let repeatedIds = map head $ filter (\ids' -> length ids' > 1) $ group ids in
          if null repeatedIds
            then Nothing
            else Just $ map RepeatedIdentifierInSVD repeatedIds
  weed (Assign ids exps) = weedAssignmentLength ids exps
  weed _ = Nothing


--
instance Weedable Variable where
  weed (Variable _ _ []) = Nothing
  weed (Variable ids _ exps) = weedAssignmentLength ids exps


--
instance Weedable IfStmt where
  weedCtxt ctxt (IfStmt _ _ stmts Nothing) = weedListCtxt ctxt stmts
  weedCtxt ctxt (IfStmt _ _ stmts (Just (Left i))) =
    weedListCtxt ctxt stmts `mappend` weedCtxt ctxt i
  weedCtxt ctxt (IfStmt _ _ stmts (Just (Right stmts'))) =
    weedListCtxt ctxt stmts `mappend` weedListCtxt ctxt stmts'


--
instance Weedable Clause where
  weedCtxt ctxt (Case _ stmts) = weedListCtxt ctxt stmts
  weedCtxt ctxt (Default stmts) = weedListCtxt ctxt stmts


-- Verifies that the number of identifiers is equal to the number of exp
-- in any type of assignment
weedAssignmentLength :: [Identifier] -> [Expression] -> Maybe [WeederError]
weedAssignmentLength ids exps =
  let (numIds, numExps) = (length ids, length exps) in
    if numIds == numExps
      then Nothing
      else Just [AssignmentCountMismatch numIds numExps]
