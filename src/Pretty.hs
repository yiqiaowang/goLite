{-# LANGUAGE FlexibleInstances #-}

module Pretty
  ( pretty
  ) where

import Data.Char (chr)
import Data.List (intercalate)
import Language

class Pretty a where
  pretty :: a -> Integer -> String
  prettyList :: [a] -> Integer -> String
  prettyList ps i = concatMap (`pretty` i) ps

commaSepList
  :: Pretty a
  => [a] -> Integer -> String
commaSepList string i = intercalate ", " (map (`pretty` i) string)

structList :: ([Identifier], Type) -> Integer -> String
structList (idList, t) i =
  concat [spacePrint i, commaSepList idList i, " ", pretty t i, ";\n"]

instance Pretty String where
  pretty s _ = s

instance Pretty Program where
  pretty (Program package alls) _ =
    concat ["package ", package, ";\n", "\n", prettyList alls 0]

instance Pretty Type where
  pretty (Type s) _ = s
  pretty (Array t expr) _ = concat ["[", pretty expr 0, "]", pretty t 0]
  pretty (Slice t) _ = concat ["[]", pretty t 0]
  pretty (Struct list) i =
    concat ["struct {\n", concatMap (`structList` (i + 1)) list, "}"]

instance Pretty Literal where
  pretty (Int' i) _ = show i
  pretty (Float64 f) _ = show f
  pretty (Rune i) _ = (show . chr . fromIntegral) i
  pretty (String s) _ = s
  pretty (Raw s) _ = s

instance Pretty Parameter where
  pretty (Parameter idList t) i =
    concat [commaSepList idList i, " ", pretty t 0]

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

spacePrint :: Integer -> String
spacePrint x =
  case x <= 0 of
    True -> ""
    False -> concat ["\t", spacePrint (x - 1)]

instance Pretty IfStmt where
  pretty (IfStmt Nothing expr stList Nothing) i =
    concat
      [ "if "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (IfStmt Nothing expr stList (Just (Right elseStmt))) i =
    concat
      [ "if "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "} else {\n"
      , prettyList elseStmt (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (IfStmt Nothing expr stList (Just (Left ifStmt))) i =
    concat
      [ "if "
      , pretty expr 0
      , " {\n"
      , prettyList stList (i + 1)
      , spacePrint i
      , "} else "
      , pretty ifStmt i
      ]
  pretty (IfStmt (Just st) expr stList Nothing) i =
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
  pretty (IfStmt (Just st) expr stList (Just (Right elseStmt))) i =
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
  pretty (IfStmt (Just st) expr stList (Just (Left ifStmt))) i =
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

instance Pretty All where
  pretty (Stmt s) _ = pretty s 0
  pretty (Function name params Nothing stmts) _ =
    concat
      [ "func "
      , pretty name 0
      , "("
      , commaSepList params 0
      , ") {\n"
      , prettyList stmts 1
      , "}\n"
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

instance Pretty Stmt where
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
  pretty (SimpleStmt simp) i = concat [spacePrint i, pretty simp i, ";\n"]
  pretty (Print expr) i =
    concat [spacePrint i, "print(", commaSepList expr i, ");\n"]
  pretty (Println expr) i =
    concat [spacePrint i, "println(", commaSepList expr i, ");\n"]
  pretty (Return Nothing) i = concat [spacePrint i, "return;\n"]
  pretty (Return (Just expr)) i =
    concat [spacePrint i, "return ", pretty expr 0, ";\n"]
  pretty (If ifstmt) i = concat [spacePrint i, pretty ifstmt i]
  pretty (Switch Nothing Nothing c) i =
    concat
      [spacePrint i, "switch {\n", prettyList c (i + 1), spacePrint i, "}\n"]
  pretty (Switch (Just stmt) Nothing c) i =
    concat
      [ spacePrint i
      , "switch "
      , pretty stmt 0
      , "; {\n"
      , prettyList c (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (Switch Nothing (Just expr) c) i =
    concat
      [ spacePrint i
      , "switch "
      , pretty expr 0
      , " {\n"
      , prettyList c (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (Switch (Just stmt) (Just expr) c) i =
    concat
      [ spacePrint i
      , "switch"
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
  pretty (For Nothing expr Nothing stmts) i =
    concat
      [ spacePrint i
      , "for ; "
      , pretty expr 0
      , "; {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (For (Just simp1) expr Nothing stmts) i =
    concat
      [ spacePrint i
      , "for "
      , pretty simp1 0
      , "; "
      , pretty expr 0
      , "; {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (For Nothing expr (Just simp2) stmts) i =
    concat
      [ spacePrint i
      , "for ; "
      , pretty expr 0
      , "; "
      , pretty simp2 i
      , " {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (For (Just simp1) expr (Just simp2) stmts) i =
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
  pretty Break i = concat [spacePrint i, "break;\n"]
  pretty Continue i = concat [spacePrint i, "continue;\n"]

instance Pretty SimpleStmt where
  pretty (ExprStmt expr) i = (pretty expr i)
  pretty (Incr ident) i = concat [pretty ident i, "++"]
  pretty (Decr ident) i = concat [pretty ident i, "--"]
  pretty (Assign idList exprList) i =
    concat [commaSepList idList i, " = ", commaSepList exprList i]
  pretty (PlusEq ident expr) i = concat [pretty ident i, " += ", pretty expr i]
  pretty (MinusEq ident expr) i = concat [pretty ident i, " -= ", pretty expr i]
  pretty (MulEq ident expr) i = concat [pretty ident i, " *= ", pretty expr i]
  pretty (DivEq ident expr) i = concat [pretty ident i, " /= ", pretty expr i]
  pretty (ModEq ident expr) i = concat [pretty ident i, " %= ", pretty expr i]
  pretty (BitAndEq ident expr) i =
    concat [pretty ident i, " &= ", pretty expr i]
  pretty (BitOrEq ident expr) i = concat [pretty ident i, " |= ", pretty expr i]
  pretty (BitXOrEq ident expr) i =
    concat [pretty ident i, " ^= ", pretty expr i]
  pretty (BitLShiftEq ident expr) i =
    concat [pretty ident i, " <<= ", pretty expr i]
  pretty (BitRShiftEq ident expr) i =
    concat [pretty ident i, " >>= ", pretty expr i]
  pretty (BitClearEq ident expr) i =
    concat [pretty ident i, " &^= ", pretty expr i]
  pretty (ShortVarDec idList exprList) i =
    concat [commaSepList idList i, " := ", commaSepList exprList i]

wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

wrapSquareList
  :: Pretty a
  => [a] -> Integer -> String
wrapSquareList xs i = concatMap wrapSquare (map (`pretty` i) xs)

instance Pretty Expression where
  pretty (Brack expr) i = concat ["(", pretty expr i, ")"]
  pretty (Id ident) i = pretty ident i
  pretty (Literal lit) i = pretty lit i
  pretty (UnaryPos expr) i = concat ["+", pretty expr i]
  pretty (UnaryNeg expr) i = concat ["-", pretty expr i]
  pretty (BoolNot expr) i = concat ["!", pretty expr i]
  pretty (BitComplement expr) i = concat ["^", pretty expr i]
  pretty (Or expr1 expr2) i = concat [pretty expr1 i, " || ", pretty expr2 i]
  pretty (And expr1 expr2) i = concat [pretty expr1 i, " && ", pretty expr2 i]
  pretty (Equals expr1 expr2) i =
    concat [pretty expr1 i, " == ", pretty expr2 i]
  pretty (NotEquals expr1 expr2) i =
    concat [pretty expr1 i, " != ", pretty expr2 i]
  pretty (LThan expr1 expr2) i = concat [pretty expr1 i, " < ", pretty expr2 i]
  pretty (LEThan expr1 expr2) i =
    concat [pretty expr1 i, " <= ", pretty expr2 i]
  pretty (GThan expr1 expr2) i = concat [pretty expr1 i, " > ", pretty expr2 i]
  pretty (GEThan expr1 expr2) i =
    concat [pretty expr1 i, " >= ", pretty expr2 i]
  pretty (Add expr1 expr2) i = concat [pretty expr1 i, " + ", pretty expr2 i]
  pretty (Sub expr1 expr2) i = concat [pretty expr1 i, " - ", pretty expr2 i]
  pretty (Mult expr1 expr2) i = concat [pretty expr1 i, " * ", pretty expr2 i]
  pretty (Div expr1 expr2) i = concat [pretty expr1 i, " / ", pretty expr2 i]
  pretty (Mod expr1 expr2) i = concat [pretty expr1 i, " % ", pretty expr2 i]
  pretty (BitAnd expr1 expr2) i = concat [pretty expr1 i, " & ", pretty expr2 i]
  pretty (BitOr expr1 expr2) i = concat [pretty expr1 i, " | ", pretty expr2 i]
  pretty (BitXor expr1 expr2) i = concat [pretty expr1 i, " ^ ", pretty expr2 i]
  pretty (BitLShift expr1 expr2) i =
    concat [pretty expr1 i, " << ", pretty expr2 i]
  pretty (BitRShift expr1 expr2) i =
    concat [pretty expr1 i, " >> ", pretty expr2 i]
  pretty (BitClear expr1 expr2) i =
    concat [pretty expr1 i, " &^ ", pretty expr2 i]
  pretty (FuncCall ident exprList) i =
    concat [pretty ident i, "(", commaSepList exprList i, ")"]
  pretty (Append ident expr) i =
    concat ["append(", pretty ident i, ", ", pretty expr i, ")"]

instance Pretty (Maybe Expression) where
  pretty Nothing i = ""
  pretty (Just e) i = pretty e i

instance Pretty Integer where
  pretty int _ = (show int)

instance Pretty Int where
  pretty int _ = (show int)

instance Pretty Identifier where
  pretty (IdOrType s) i = s
  pretty (IdArray s xs) i = concat [s, wrapSquareList (map (`pretty` 0) xs) i]
  pretty (IdField xs) i = intercalate "." $ map (`pretty` i) xs

dotSepList :: [String] -> String
dotSepList string = intercalate "." string
