module Weeder
  ( WeederError(..)
  , weed
  ) where


import Language
import Data.Maybe
import Control.Monad(join)


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
  weed :: a -> Maybe WeederError

  weedList :: [a] -> Maybe WeederError
  weedList xs = join $ head' $ filter isJust $ map weed xs


--
instance Weedable Program where
  weed (Program package alls) = weedList alls


--
instance Weedable All where
  weed s@(Stmt stmt) = weed stmt
  weed f@(Function _ _ _ stmts) = weedList stmts


--
instance Weedable Stmt where
  weed v@(VarDec var) = weed var
  weed _ = Nothing


--
instance Weedable Variable where
  weed v@(Variable _ _ []) = Nothing
  weed v@(Variable ids _ exps) =
    let (numIds, numExps) = (length ids, length exps) in
      if numIds == numExps
        then Nothing
        else Just $ MismatchingVariableDeclaration numIds numExps


--
instance Weedable Clause where
  weed c@(Case exps stmts) = Nothing
  weed d@(Default stmts) = Nothing


head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x
