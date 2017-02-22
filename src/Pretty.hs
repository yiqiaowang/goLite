module Pretty
  ( pretty
  ) where


import Data.Char(chr)
import Data.List(intercalate)
import Language


class Pretty a where
  pretty :: a -> String

  prettyList :: [a] -> String
  prettyList = concatMap pretty


instance Pretty Program where
  pretty (Program package alls) = concat
    [ "package ", package, ";\n"
    , "\n"
    , prettyList alls
    ]


instance Pretty Literal where
  pretty (Int i) = show i
  pretty (Float64 f) = show f
  pretty (Rune i) = (show . chr . fromIntegral) i
  pretty (String s) = s
  pretty (Raw s) = s


instance Pretty Parameter where
  pretty _ = undefined


instance Pretty Clause where
  pretty _ = undefined


instance Pretty IfStmt where
  pretty _ = undefined


instance Pretty All where
  pretty (Stmt s) = pretty s
  pretty (Function name params mt stmts) = undefined


instance Pretty Variable where
  pretty _ = undefined


instance Pretty TypeName where
  pretty _ = undefined


instance Pretty Stmt where
  pretty _ = undefined


instance Pretty SimpleStmt where
  pretty _ = undefined


instance Pretty Expression where
  pretty _ = undefined
