module Pretty
  ( pretty
  ) where


import Data.Char(chr)
import Data.List(intercalate)
import Language


class Pretty a where
  pretty :: a -> Int -> String

  prettyList :: [a] -> Int -> String
  prettyList ps i = concatMap (`pretty` i)

commaSepList :: [a] -> String
commaSepList string = intercalate ", " (pretty string)

instance Pretty Program where
  pretty (Program package alls) _ = concat
    [ "package ", package, ";\n"
    , "\n"
    , prettyList alls 0
    ]

instance Pretty Literal where
  pretty (Int i) _ = show i
  pretty (Float64 f) _ = show f
  pretty (Rune i) _ = (show . chr . fromIntegral) i
  pretty (String s) _ = s
  pretty (Raw s) _ = s

instance Pretty Parameter where
  pretty (Parameter idList t) _ = concat 
    [commaSepList idList, " ", pretty t]

instance Pretty Clause where
  pretty (Case exList stList) i = concat 
    ["case ", commaSepList exList, ":\n", 
    prettyList stList (i+1)]
  pretty (Default stList) i = concat
    ["default:\n" prettyList stList (i+1)]

spacePrint :: Int -> String
spacePrint x = case x <= 0 of
                 True -> ""
                 False -> concat["\t", spacePrint (x-1)]

instance Pretty IfStmt where
  pretty (IfStmt Nothing expr stList Nothing) i = 
    concat ["if ", pretty expr 0, " {\n", 
    prettyList stList (i+1), spacePrint i, "}\n"]
  pretty (IfStmt Nothing expr stList (Just (Right elseStmt))) i = 
    concat ["if ", pretty expr 0, " {\n", 
    prettyList stList (i+1), spacePrint i, "} else {\n",
    prettyList elseStmt (i+1), spacePrint i, "}\n"]
  pretty (IfStmt Nothing expr stList (Just(Left ifStmt))) i = 
    concat ["if ", pretty expr 0, " {\n", 
    prettyList stList (i+1), spacePrint i, 
    "} else " pretty ifstmt]
  pretty (IfStmt (Just st) expr stList Nothing) i = 
    concat ["if ", pretty st 0, "; ", pretty expr 0, 
    " {\n", prettyList stList (i+1), 
    spacePrint i, "}\n"]
  pretty (IfStmt (Just st) expr stList (Just (Right elseStmt))) i = 
    concat ["if ", pretty st 0, "; ", pretty expr 0, " {\n", 
    prettyList stList (i+1), spacePrint i, "} else {\n",
    prettyList elseStmt (i+1), spacePrint i, "}\n"]
  pretty (IfStmt (Just st) expr stList (Just(Left ifStmt))) i = 
    concat ["if ", pretty st 0, "; ", pretty expr 0,
    " {\n", prettyList stList (i+1), spacePrint i, 
    "} else " pretty ifstmt]

instance Pretty All where
  pretty (Stmt s) _ = pretty s 0
  pretty (Function name params Nothing stmts) _ = 
    concat ["func ", pretty name 0, "("
    pretty params 0, ") {\n", 
    pretty stmts 1, "}\n"]
  pretty (Function name params (Just t) stmts) _ = 
    concat ["func ", pretty name 0, "("
    pretty params 0, ") ", pretty t,
    " {\n", pretty stmts 1, "}\n"]

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
