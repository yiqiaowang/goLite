{-# LANGUAGE FlexibleInstances #-}

module Pretty.Pretty
  ( pretty
  ) where


import Data.List(intercalate)
import Data.Char(chr)
import Language


class Pretty a where
  pretty :: a -> Integer -> String

  prettyList :: [a] -> Integer -> String
  prettyList ps i = concatMap (`pretty` i) ps


--
structList :: ([Identifier], Type) -> Integer -> String
structList (idList, t) i =
  concat [spacePrint i, commaSepList idList i, " ", pretty t i, ";\n"]

--
commaSepList :: Pretty a => [a] -> Integer -> String
commaSepList string i = intercalate ", " (map (`pretty` i) string)

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: Pretty a => [a] -> Integer -> String
wrapSquareList xs i = concatMap wrapSquare (map (`pretty` i) xs)

--
dotSepList :: [String] -> String
dotSepList = intercalate "."


instance Pretty Program where
  pretty (Program package alls) _ =
    concat ["package ", package, ";\n", "\n", prettyList alls 0]

instance Pretty All where
  pretty (TopDec dec) _ = pretty dec 0
  pretty (Function name params Nothing stmts) _ =
    concat
      [ "func "
      , pretty name 0
      , "("
      , commaSepList params 0
      , ") {\n"
      , prettyList stmts 1
      , "}\n\n"
      ]
  pretty (Function name params (Just t) stmts) _ =
    concat
      [ "func "
      , pretty name 0
      , "("
      , commaSepList params 0
      , ") "
      , pretty t 0
      , " {\n"
      , prettyList stmts 1
      , "}\n\n"
      ]

instance Pretty TopLevel where
  pretty (VarDec (Variable var (Just t) [])) i =
    concat [spacePrint i, "var ", commaSepList var i, " ", pretty t i, ";\n"]
  pretty (VarDec (Variable var (Just t) expr)) i =
    concat
      [ spacePrint i
      , "var "
      , commaSepList var i
      , " "
      , pretty t i
      , " = "
      , commaSepList expr i
      , ";\n"
      ]
  pretty (VarDec (Variable var Nothing expr)) i =
    concat
      [ spacePrint i
      , "var "
      , commaSepList var i
      , " = "
      , commaSepList expr i
      , ";\n"
      ]
  pretty (VarDecList vList) i =
    concat
      [spacePrint i, "var (\n", prettyList vList (i + 1), spacePrint i, ");\n"]
  pretty (TypeDec (TypeName ident t)) i =
    concat [spacePrint i, "type ", pretty ident i, " ", pretty t i, ";\n"]
  pretty (TypeDecList tList) i =
    concat
      [spacePrint i, "type (\n", prettyList tList (i + 1), spacePrint i, ");\n"]

instance Pretty Variable where
  pretty (Variable var (Just t) []) i =
    concat [spacePrint i, commaSepList var i, " ", pretty t i, ";\n"]
  pretty (Variable var (Just t) expr) i =
    concat
      [ spacePrint i
      , commaSepList var i
      , " "
      , pretty t i
      , " = "
      , commaSepList expr i
      , ";\n"
      ]
  pretty (Variable var Nothing expr) i =
    concat [spacePrint i, commaSepList var i, " = ", commaSepList expr i, ";\n"]

instance Pretty TypeName where
  pretty (TypeName ident t) i =
    concat [spacePrint i, pretty ident i, " ", pretty t i, ";\n"]

instance Pretty Identifier where
  pretty (IdOrType s) i = s
  pretty (IdArray s xs) i = concat [s, wrapSquareList (map (`pretty` 0) xs) i]
  pretty (IdField xs) i = intercalate "." $ map (`pretty` i) xs

instance Pretty Type where
  pretty (Alias s) _ = s
  pretty (Array t expr) _ = concat ["[", pretty expr 0, "]", pretty t 0]
  pretty (Slice t) _ = concat ["[]", pretty t 0]
  pretty (Struct list) i =
    concat ["struct {\n", concatMap (`structList` (i + 1)) list, "}"]

instance Pretty Parameter where
  pretty (Parameter idList t) i =
    concat [commaSepList idList i, " ", pretty t 0]

instance Pretty Stmt where
  pretty (StmtDec dec) i = pretty dec i
  pretty (SimpleStmt simp) i = concat [spacePrint i, pretty simp i, ";\n"]
  pretty (Print expr) i =
    concat [spacePrint i, "print(", commaSepList expr i, ");\n"]
  pretty (Println expr) i =
    concat [spacePrint i, "println(", commaSepList expr i, ");\n"]
  pretty (Return Nothing) i = concat [spacePrint i, "return;\n"]
  pretty (Return (Just expr)) i =
    concat [spacePrint i, "return ", pretty expr 0, ";\n"]
  pretty (If ifstmt) i = concat [spacePrint i, pretty ifstmt i]
  pretty (Switch stmt Nothing c) i =
    concat
      [ spacePrint i
      , "switch "
      , pretty stmt 0
      , "; {\n"
      , prettyList c (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (Switch stmt (Just expr) c) i =
    concat
      [ spacePrint i
      , "switch "
      , pretty stmt 0
      , "; "
      , pretty expr 0
      , " {\n"
      , prettyList c (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (Infinite stmts) i =
    concat
      [spacePrint i, "for {\n", prettyList stmts (i + 1), spacePrint i, "}\n"]
  pretty (While expr stmts) i =
    concat
      [ spacePrint i
      , "for "
      , pretty expr 0
      , " {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (For simp1 expr simp2 stmts) i =
    concat
      [ spacePrint i
      , "for "
      , pretty simp1 0
      , "; "
      , pretty expr 0
      , "; "
      , pretty simp2 0
      , " {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (Block xs) i = concat [spacePrint i, "{\n", prettyList xs (i+1), spacePrint i, "};\n"]
  pretty Break i = concat [spacePrint i, "break;\n"]
  pretty Continue i = concat [spacePrint i, "continue;\n"]

instance Pretty SimpleStmt where
  pretty (StmtFuncCall func) i = (pretty func i)
  pretty (Incr ident) i = concat [pretty ident i, "++"]
  pretty (Decr ident) i = concat [pretty ident i, "--"]
  pretty (Assign idList exprList) i =
    concat [commaSepList idList i, " = ", commaSepList exprList i]
  pretty (ShortBinary opEq ident expr) i = concat [pretty ident i, " ", pretty opEq i, " ", pretty expr i]
  pretty (ShortVarDec idList exprList) i =
    concat [commaSepList idList i, " := ", commaSepList exprList i]
  pretty (EmptyStmt) i = ""

instance Pretty FunctionCall where
  pretty (FunctionCall ident exprList) i =
    concat [pretty ident i, "(", commaSepList exprList i, ")"]

instance Pretty IfStmt where
  pretty (IfStmt st expr stList (IfStmtCont Nothing)) i =
    concat
      [ "if "
      , pretty st 0
      , "; "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (IfStmt st expr stList (IfStmtCont (Just (Right elseStmt)))) i =
    concat
      [ "if "
      , pretty st 0
      , "; "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "} else {\n"
      , prettyList elseStmt (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (IfStmt st expr stList (IfStmtCont (Just (Left ifStmt)))) i =
    concat
      [ "if "
      , pretty st 0
      , "; "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "} else "
      , pretty ifStmt i
      ]

instance Pretty Clause where
  pretty (Case exList stList) i =
    concat
      [ spacePrint i
      , "case "
      , commaSepList exList i
      , ":\n"
      , prettyList stList (i + 1)
      ]
  pretty (Default stList) i =
    concat [spacePrint i, "default:\n", prettyList stList (i + 1)]

instance Pretty Expression where
  pretty (Brack expr) i = concat ["(", pretty expr i, ")"]
  pretty (Id ident) i = pretty ident i
  pretty (Literal lit) i = pretty lit i
  pretty (Unary op expr) i = concat [pretty op i, pretty expr i]
  pretty (Binary op expr1 expr2) i = concat [pretty expr1 i, " ", pretty op i, " ", pretty expr2 i]
  pretty (ExprFuncCall func) i = pretty func i
  pretty (Append ident expr) i =
    concat ["append(", pretty ident i, ", ", pretty expr i, ")"]

--
instance Pretty UnaryOp where
  pretty Pos _ = "+"
  pretty Neg _ = "-"
  pretty BoolNot _ = "!"
  pretty BitComplement _ = "^"

--
instance Pretty BinaryOp where
  pretty Or _ = "||"
  pretty And _ = "&&"
  pretty Equals _ = "=="
  pretty NotEquals _ = "!="
  pretty LThan _ = "<"
  pretty LEThan _ = "<="
  pretty GThan _ = ">"
  pretty GEThan _ = ">="
  pretty Add _ = "+"
  pretty Sub _ = "-"
  pretty Mult _ = "*"
  pretty Div _ = "/"
  pretty Mod _ = "%"
  pretty BitAnd _ = "&"
  pretty BitOr _ = "|"
  pretty BitXor _ = "^"
  pretty BitLShift _ = "<<"
  pretty BitRShift _ = ">>"
  pretty BitClear _ = "&^"

instance Pretty BinaryOpEq where
  pretty PlusEq _ = "+="
  pretty MinusEq _ = "-="
  pretty MulEq _ = "*="
  pretty DivEq _ = "/="
  pretty ModEq _ = "%="
  pretty BitAndEq _ = "&="
  pretty BitOrEq _ = "|="
  pretty BitXorEq _ = "^="
  pretty BitLShiftEq _ = "<<="
  pretty BitRShiftEq _ = ">>="
  pretty BitClearEq _ = "&^="

instance Pretty (Maybe Expression) where
  pretty Nothing i = ""
  pretty (Just e) i = pretty e i

instance Pretty String where
  pretty s _ = s


instance Pretty Integer where
  pretty int _ = (show int)


instance Pretty Int where
  pretty int _ = (show int)

--
instance Pretty Literal where
  pretty (Int' i) _ = show i
  pretty (Float64 f) _ = show f
  pretty (Rune i) _ = (show . chr . fromIntegral) i
  pretty (String s) _ = s
  pretty (Raw s) _ = s
