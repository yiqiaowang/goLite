{-# LANGUAGE FlexibleInstances #-}

module Pretty.TypedLanguage
  ( pretty
  ) where


import Data.List (intercalate)
import Pretty.Util
import Pretty.Operators
import Pretty.Pretty
import Pretty.Common
import Language.TypedLanguage
import Language.Operators
import Language.Common


--
structList :: ([TIdentifier], TType) -> Integer -> String
structList (idList, t) i =
  concat [spacePrint i, commaSepList idList i, " ", pretty t i, ";\n"]

-- Used to comment types while pretty printing.
blockComment :: TType -> String
blockComment p = "/*" ++ pretty p ++ "*/"

instance Pretty TProgram where
  pretty (TProgram package alls) _ =
    concat ["package ", package, ";\n", "\n", prettyList alls 0]

instance Pretty TAll where
  pretty (TTopDec dec) _ = pretty dec 0
  pretty (TFunction name params Nothing stmts) _ =
    concat
      [ "func "
      , pretty name 0
      , "("
      , commaSepList params 0
      , ") {\n"
      , prettyList stmts 1
      , "}\n\n"
      ]
  pretty (TFunction name params (Just t) stmts) _ =
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

instance Pretty TTopLevel where
  pretty (TVarDec (TVariable var (Just t) [])) i =
    concat [spacePrint i, "var ", commaSepList var i, " ", pretty t i, ";\n"]
  pretty (TVarDec (TVariable var (Just t) expr)) i =
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
  pretty (TVarDec (TVariable var Nothing expr)) i =
    concat
      [ spacePrint i
      , "var "
      , commaSepList var i
      , " = "
      , commaSepList expr i
      , ";\n"
      ]
  pretty (TVarDecList vList) i =
    concat
      [spacePrint i, "var (\n", prettyList vList (i + 1), spacePrint i, ");\n"]
  pretty (TTypeDec (TTypeName ident t)) i =
    concat [spacePrint i, "type ", pretty ident i, " ", pretty t i, ";\n"]
  pretty (TTypeDecList tList) i =
    concat
      [spacePrint i, "type (\n", prettyList tList (i + 1), spacePrint i, ");\n"]

instance Pretty TVariable where
  pretty (TVariable var (Just t) []) i =
    concat [spacePrint i, commaSepList var i, " ", pretty t i, ";\n"]
  pretty (TVariable var (Just t) expr) i =
    concat
      [ spacePrint i
      , commaSepList var i
      , " "
      , pretty t i
      , " = "
      , commaSepList expr i
      , ";\n"
      ]
  pretty (TVariable var Nothing expr) i =
    concat [spacePrint i, commaSepList var i, " = ", commaSepList expr i, ";\n"]

instance Pretty TTypeName where
  pretty (TTypeName ident t) i =
    concat [spacePrint i, pretty ident i, " ", pretty t i, ";\n"]

instance Pretty TIdentifier where
  pretty (TIdOrType s) i = s
  pretty (TIdArray s xs) i = concat [s, wrapSquareList (map (`pretty` 0) xs) i]
  pretty (TIdField xs) i = intercalate "." $ map (`pretty` i) xs

instance Pretty TType where
  pretty (TAlias s) _ = s
  pretty (TArray t expr) _ = concat ["[", pretty expr 0, "]", pretty t 0]
  pretty (TSlice t) _ = concat ["[]", pretty t 0]
  pretty (TStruct list) i =
    concat ["struct {\n", concatMap (`structList` (i + 1)) list, "}"]

instance Pretty TParameter where
  pretty (TParameter idList t) i =
    concat [commaSepList idList i, " ", pretty t 0]

instance Pretty TStmt where
  pretty (TStmtDec dec) i = pretty dec i
  pretty (TSimpleStmt simp) i = concat [spacePrint i, pretty simp i, ";\n"]
  pretty (TPrint expr) i =
    concat [spacePrint i, "print(", commaSepList expr i, ");\n"]
  pretty (TPrintln expr) i =
    concat [spacePrint i, "println(", commaSepList expr i, ");\n"]
  pretty (TReturn Nothing) i = concat [spacePrint i, "return;\n"]
  pretty (TReturn (Just expr)) i =
    concat [spacePrint i, "return ", pretty expr 0, ";\n"]
  pretty (TIf ifstmt) i = concat [spacePrint i, pretty ifstmt i]
  pretty (TSwitch stmt Nothing c) i =
    concat
      [ spacePrint i
      , "switch "
      , pretty stmt 0
      , "; {\n"
      , prettyList c (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (TSwitch stmt (Just expr) c) i =
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
  pretty (TInfinite stmts) i =
    concat
      [spacePrint i, "for {\n", prettyList stmts (i + 1), spacePrint i, "}\n"]
  pretty (TWhile expr stmts) i =
    concat
      [ spacePrint i
      , "for "
      , pretty expr 0
      , " {\n"
      , prettyList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  pretty (TFor simp1 expr simp2 stmts) i =
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
  pretty (TBlock xs) i = concat [spacePrint i, "{\n", prettyList xs (i+1), spacePrint i, "};\n"]
  pretty TBreak i = concat [spacePrint i, "break;\n"]
  pretty TContinue i = concat [spacePrint i, "continue;\n"]

instance Pretty TSimpleStmt where
  pretty (TStmtFuncCall func) i = (pretty func i)
  pretty (TIncr ident) i = concat [pretty ident i, "++"]
  pretty (TDecr ident) i = concat [pretty ident i, "--"]
  pretty (TAssign idList exprList) i =
    concat [commaSepList idList i, " = ", commaSepList exprList i]
  pretty (TShortBinary opEq ident expr) i = concat [pretty ident i, " ", pretty opEq i, " ", pretty expr i]
  pretty (TShortVarDec idList exprList) i =
    concat [commaSepList idList i, " := ", commaSepList exprList i]
  pretty (TEmptyStmt) i = ""

instance Pretty TFunctionCall where
  pretty (TFunctionCall ident exprList) i =
    concat [pretty ident i, "(", commaSepList exprList i, ")"]

instance Pretty TIfStmt where
  pretty (TIfStmt st expr stList (TIfStmtCont Nothing)) i =
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
  pretty (TIfStmt st expr stList (TIfStmtCont (Just (Right elseStmt)))) i =
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
  pretty (TIfStmt st expr stList (TIfStmtCont (Just (Left ifStmt)))) i =
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

instance Pretty TClause where
  pretty (TCase exList stList) i =
    concat
      [ spacePrint i
      , "case "
      , commaSepList exList i
      , ":\n"
      , prettyList stList (i + 1)
      ]
  pretty (TDefault stList) i =
    concat [spacePrint i, "default:\n", prettyList stList (i + 1)]

instance Pretty TExpression where
  pretty ((TBrack expr), type') i = concat ["(", pretty expr i, ")"]
  pretty ((TId ident), type') i = pretty ident i ++ blockComment type'
  pretty ((TLiteral lit), type') i = pretty lit i ++ blockComment type'
  pretty ((TUnary op expr), type') i =
    concat [pretty op i, pretty expr i, blockComment type']
  pretty ((TBinary op expr1 expr2), type') i =
    concat [pretty expr1 i, " ", pretty op i, " ", pretty expr2 i, blockComment type']
  pretty ((TExprFuncCall func), type') i = pretty func i ++ blockComment type'
  pretty ((TAppend ident expr), type') i =
    concat ["append(", pretty ident i, ", ", pretty expr i, ")", blockComment type']

instance Pretty (Maybe TExpression) where
  pretty Nothing i = ""
  pretty (Just (expr, type')) i = pretty expr i ++ blockComment type'
