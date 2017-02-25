module Weeder
  ( WeederError(..)
  , weed
  ) where

import Data.List (group)
import Language

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
  weed (Stmt stmt) = weed stmt
  weed (Function (IdOrType "main") _ _ stmts) = weedListCtxt [CFunction] stmts
  weed (Function _ _ _ stmts) =
    weedListCtxt [CFunction] stmts `mappend`
    if any isReturn stmts
      then Nothing
      else Just [MissingReturn]
    where
      isReturn (Return _) = True
      isReturn (If a) = (ifReturn a)
      isReturn (Switch _ _ a) = (clauseReturn a)
      isReturn _ = False

hasReturn :: [Stmt] -> Bool
hasReturn [] = False
hasReturn ((Return _):xs) = True
hasReturn ((If a):[]) = (ifReturn a)
hasReturn ((Switch _ _ a):[]) = (clauseReturn a)
hasReturn ((If a):xs) = ((ifReturn a) || (hasReturn xs))
hasReturn ((Switch _ _ a):xs) = ((clauseReturn a) || (hasReturn xs))

ifReturn :: IfStmt -> Bool
ifReturn (IfStmt _ _ a Nothing) = (hasReturn a)
ifReturn (IfStmt _ _ a (Just (Right b))) = ((hasReturn a) && (hasReturn b))
ifReturn (IfStmt _ _ a (Just (Left c))) = ((hasReturn a) && (ifReturn c))

clauseReturn :: [Clause] -> Bool
clauseReturn [] = False
clauseReturn ((Case _ a):[]) = (hasReturn a)
clauseReturn ((Default a):[]) = (hasReturn a)
clauseReturn ((Case _ a):xs) = ((hasReturn a) && (clauseReturn xs))
clauseReturn ((Default a):xs) = ((hasReturn a) && (clauseReturn xs))

--
instance Weedable Stmt where
  weedCtxt ctxt (If i) = weedCtxt ctxt i
  weedCtxt ctxt (Switch _ _ clauses) =
    weedListCtxt ctxt clauses `mappend`
    let n = length $ filter isDefault clauses
    in if n > 1
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
  weedCtxt ctxt (SimpleStmt stmt) = weedCtxt ctxt stmt `mappend` weed stmt
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
  weedCtxt _ (VarDec var) = weed var
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
    weedAssignmentLength ids exps `mappend`
    let repeatedIds = map head $ filter (\ids' -> length ids' > 1) $ group ids
    in if null repeatedIds
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
  let (numIds, numExps) = (length ids, length exps)
  in if numIds == numExps
       then Nothing
       else Just [AssignmentCountMismatch numIds numExps]
