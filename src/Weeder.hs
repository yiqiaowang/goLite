module Weeder
  ( WeederError(..)
  , weed
  ) where


import Language
import Data.Maybe
import Control.Monad(join)
import Control.Applicative((<|>))


--
data WeederError
  = ExcessiveDefaults
    { numDefaults :: Int }
  | MismatchingVariableDeclaration
    { numExpected :: Int
    , numFound :: Int
    }
  | InvalidBreak
  | InvalidContinue
  | InvalidReturn
  | MissingReturn
  deriving (Eq, Show)


--
data Context
  = CFunction
  | CLoop
  deriving Eq


--
class Weedable a where
  weed :: a -> Maybe WeederError
  weed = weedCtxt []

  weedCtxt :: [Context] -> a -> Maybe WeederError
  weedCtxt ctxt = weed

  weedListCtxt :: [Context] -> [a] -> Maybe WeederError
  weedListCtxt ctxt ws = join $ head' $ filter isJust $ map (weedCtxt ctxt) ws

--
instance Weedable Program where
  weed (Program package alls) = weedListCtxt [] alls


--
instance Weedable All where
  weed (Stmt stmt) = weed stmt
  weed (Function "main" _ _ stmts) =
    weedListCtxt [] stmts
  weed (Function _ _ _ stmts) =
    weedListCtxt [CFunction] stmts <|>
      if any isReturn stmts
        then Nothing
        else Just MissingReturn
      where
        isReturn (Return _) = True
        isReturn _ = False


--
instance Weedable Stmt where
  weedCtxt ctxt (If i) = weedCtxt ctxt i
  weedCtxt ctxt (Infinite stmts) = weedListCtxt (CLoop : ctxt) stmts
  weedCtxt ctxt (While _ stmts) = weedCtxt ctxt (Infinite stmts)
  weedCtxt ctxt (For _ _ _ stmts) = weedCtxt ctxt (Infinite stmts)
  weedCtxt ctxt Break =
    if CLoop `elem` ctxt
      then Nothing
      else Just InvalidBreak
  weedCtxt ctxt Continue =
    if CLoop `elem` ctxt
      then Nothing
      else Just InvalidContinue
  weedCtxt ctxt (Return _) =
    if CFunction `elem` ctxt
      then Nothing
      else Just InvalidReturn
  weedCtxt _  (VarDec var) = weed var
  weedCtxt _ _ = Nothing


--
instance Weedable Variable where
  weed (Variable _ _ []) = Nothing
  weed (Variable ids _ exps) =
    let (numIds, numExps) = (length ids, length exps) in
      if numIds == numExps
        then Nothing
        else Just $ MismatchingVariableDeclaration numIds numExps


--
instance Weedable IfStmt where
  weedCtxt ctxt (IfStmt _ _ stmts Nothing) = weedListCtxt ctxt stmts
  weedCtxt ctxt (IfStmt _ _ stmts (Just (Left i))) =
    weedListCtxt ctxt stmts <|> weedCtxt ctxt i
  weedCtxt ctxt (IfStmt _ _ stmts (Just (Right stmts'))) =
    weedListCtxt ctxt stmts <|> weedListCtxt ctxt stmts'


--
instance Weedable Clause where
  weedCtxt ctxt (Case _ stmts) = weedListCtxt ctxt stmts
  weedCtxt ctxt (Default stmts) = weedListCtxt ctxt stmts


--
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x
