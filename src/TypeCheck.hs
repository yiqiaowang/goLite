module TypeCheck where

import SymbolTable

data TypeCheckError =
  SymbolTableError

--
class TypeCheckable a where
  typecheck :: a -> SymbolTable -> Maybe [TypeCheckError]


