module Weeder
  ( WeederError(..)
  , weed
  ) where


import Language


--
data WeederError
  = ExcessiveDefaults
    { numDefaults :: Int }
  | MismatchingVariableDeclaration
    { numExpected :: Int
    , numFound :: Int
    }
  deriving (Eq, Show)


--
class Weedable a where
  weed :: a -> Either WeederError a


--
instance Weedable Program where
  weed (Program package alls) =
    case mapM weed alls of
      Left weederError -> Left weederError
      Right _ -> return $ Program package alls


--
instance Weedable All where
  weed s@(Stmt stmt) =
    case weed stmt of
      Left weederError -> Left weederError
      Right _ -> return s
  weed f@(Function i p t stmts) =
    case mapM weed stmts of
      Left weederError -> Left weederError
      Right _ -> return f


--
instance Weedable Stmt where
  weed v@(VarDec var) =
    case weed var of
      Left weederError -> Left weederError
      Right _ -> return v
  weed stmt = return stmt


--
instance Weedable Variable where
  weed v@(Variable ids _ []) = return v
  weed v@(Variable ids _ exps) =
    let (numIds, numExps) = (length ids, length exps) in
      if numIds == numExps
        then return v
        else Left $ MismatchingVariableDeclaration numIds numExps

--
instance Weedable Clause where
  weed c@(Case exps stmts) = return c
  weed d@(Default stmts) = return d
