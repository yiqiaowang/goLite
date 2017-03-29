{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGenerator
  ( Codeable(..)
  ) where


import Data.List(intercalate)
import Data.Char(chr)
import Language
import CodeGen.Comparison
import CodeGen.Array
import CodeGen.Expressions
import CodeGen.Codeable


structList :: ([Identifier], Type) -> Integer -> String -> String
structList ([], _) i s = ""
structList ((ident:idents), t) i s =
  concat
    [spacePrint i
    , s
    , "."
    , code ident i
    , " = "
    , emptyTypeValue t i (concat [s, ".", code ident i])
    , ";\n"]

structPrint :: [([Identifier], Type)] -> Integer -> String -> String
structPrint [] _ _ = ""
structPrint (struct:structs) i s = concat[structList struct i s, structPrint structs i s]

emptyTypeValue :: Type -> Integer -> String -> String
emptyTypeValue (Alias "string") _ _ = "\"\""
emptyTypeValue (Alias "bool") _ _ = "false"
emptyTypeValue (Alias "int") _ _ = "0"
emptyTypeValue (Alias "float64") _ _ = "0.0"
emptyTypeValue (Alias "rune") _ _ = "\'\\0\'"
emptyTypeValue (Alias str) _ _ = concat[str, "()"]

emptyTypeValue (Array t num) i s = concat 
                         ["new Array("
                         , code num 0
                         , ");\n"
                         , spacePrint i
                         , "for (var GO_COUNT = 0;"
                         , " GO_COUNT < "
                         , code num 0
                         , "; GO_COUNT++) {\n"
                         , spacePrint (i+1)
                         , code s 0
                         , "[GO_COUNT] = "
                         , emptyTypeValue t (i+1) s
                         , ";\n"
                         , spacePrint i
                         , "}"]

emptyTypeValue (Slice _) _ _ = "new Array()"
emptyTypeValue (Struct struct) i s =
  concat
    ["{};\n"
    , structPrint struct (i + 1) s]

varList :: [Identifier] -> (Maybe Type) -> [Expression] -> Integer -> String
varList [] exprs _ _ = ""
varList (var:[]) (Just t) [] i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , emptyTypeValue t i (code var i)
          , ";\n"]
varList (var:vars) (Just t) [] i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , emptyTypeValue t i (code var i)
          , "\n"
          , varList vars (Just t) [] i]
varList (var:[]) _ (expr:exprs) i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , code expr i
          , ";\n"]
varList (var:vars) t (expr:exprs) i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , code expr i
          , "\n"
          , varList vars t exprs i]

-- Need to do
instance Codeable Program where
  code (Program package alls) _ = concat
    [ goLiteAppend
    , goLiteEquals
    , goLiteNotEquals
    , codeList alls 0
    ]

instance Codeable All where
  code (TopDec dec) _ = concat [code dec 0, "\n"]
  code (Function name params _ stmts) _ =
    case name of
      "main" -> concat
            [ "function "
            , code name 0
            , "("
            , commaSepList params 0
            , ") {\n"
            , codeList stmts 1
            , "}\nmain();\n\n"
            ]
      _ -> concat
            [ "function "
            , code name 0
            , "("
            , commaSepList params 0
            , ") {\n"
            , codeList stmts 1
            , "}\n\n"
            ]

instance Codeable TopLevel where
  code (VarDec var) i = code var i
  code (VarDecList vList) i = codeList vList i
  code (TypeDec t) i = code t i
  code (TypeDecList tList) i = codeList tList i

instance Codeable Variable where
  code (Variable [] _ []) _ = ""
  code (Variable (var:[]) (Just t) []) i =
    concat
      [spacePrint i
      , "var "
      , code var i
      , " = "
      , emptyTypeValue t i (code var i)
      , ";\n"
      ]
  code (Variable (var:vars) (Just t) []) i =
    concat
      [spacePrint i
      , "var "
      , code var i
      , " = "
      , emptyTypeValue t i (code var i)
      , "\n"
      , varList vars (Just t) [] (i + 1)
      ]
  code (Variable (var:[]) _ (expr:exprs)) i =
    concat
      [spacePrint i
      , "var "
      , code var i
      , " = "
      , code expr i
      , ";\n"
      ]
  code (Variable (var:vars) t (expr:exprs)) i =
    concat
      [ spacePrint i
      , "var "
      , code var i
      , " = "
      , code expr i
      , "\n"
      , varList vars t exprs (i + 1)
      ]

instance Codeable TypeName where
  code (TypeName (Alias s1) (Alias s2)) i =
    concat
      [spacePrint i
      , "var "
      , code (Alias s1) i
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return "
      , emptyTypeValue (Alias s2) i (code (Alias s1) i)
      , ";\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Array _ num)) i =
    concat
      [spacePrint i
      , "var "
      , code (Alias s) i
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return new Array("
      , code num i
      , ");\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Slice _)) i =
    concat
      [spacePrint i
      , "var "
      , code (Alias s) i
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return new Array();\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Struct struct)) i =
    concat
      [spacePrint i
      , "var "
      , code (Alias s) i
      , " = function() {\n"
      , spacePrint (i + 1)
      , "var struct = {};\n"
      , structPrint struct (i + 1) s
      , "return struct;\n"
      , spacePrint i
      , "}\n"]
  code (TypeName _ _) i = ""

-- UNUSED!!!!!!!!!!!
instance Codeable Type where
  code (Alias s) _ = s
  code (Array t expr) _ = concat ["new Array(", code expr 0, ")"]
  code (Slice t) _ = "new Array()"
  code (Struct list) i = "{}"

instance Codeable Parameter where
  code (Parameter idList _) i = commaSepList idList i

instance Codeable Stmt where
  code (StmtDec dec) i = code dec i
  code (SimpleStmt simp) i = concat [spacePrint i, code simp i, ";\n"]
  code (Print expr) i = concat
    [spacePrint i, "process.stdout.write(JSON.stringify(", commaSepList expr i, "));\n"]
  code (Println expr) i = concat
    [spacePrint i, "console.log(", commaSpaceSepList expr i, ");\n"]
  code (Return Nothing) i = concat [spacePrint i, "return;\n"]
  code (Return (Just expr)) i =
    concat [spacePrint i, "return ", code expr 0, ";\n"]
  code (If ifstmt) i = concat [spacePrint i, code ifstmt i]
  code (Switch stmt expr c) i =
    concat
      [ spacePrint i
      , "switch ("
      , code expr 0
      , ") {\n"
      , codeList (inject c stmt) (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (Infinite stmts) i =
    concat
      [spacePrint i, "while (true) {\n", codeList stmts (i + 1), spacePrint i, "}\n"]
  code (While expr stmts) i =
    concat
      [ spacePrint i
      , "while ("
      , code expr 0
      , ") {\n"
      , codeList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (For simp1 expr simp2 stmts) i =
    concat
      [ spacePrint i
      , "for ("
      , code simp1 0
      , "; "
      , code expr 0
      , "; "
      , code simp2 0
      , ") {\n"
      , codeList stmts (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (Block xs) i = concat [spacePrint i, "{\n", codeList xs (i+1), spacePrint i, "}\n"]
  code Break i = concat [spacePrint i, "break;\n"]
  code Continue i = concat [spacePrint i, "continue;\n"]

instance Codeable SimpleStmt where
  code (StmtFuncCall func) i = (code func i)
  code (Incr ident) i = concat [code ident i, "++"]
  code (Decr ident) i = concat [code ident i, "--"]
  code (Assign [] _) i = ""
  code (Assign (ident:[]) (expr:exprList)) i =
    concat
      [ code ident i
      , " = "
      , code expr i
      ]
  code (Assign (ident:identList) (expr:exprList)) i =
    concat
      [ code ident i
      , " = "
      , code expr i
      , ", "
      , code (Assign identList exprList) i
      ]
  code (ShortBinary opEq ident expr) i = concat [code ident i, " ", code opEq i, code expr i]
  code (ShortVarDec [] _) i = ""
  code (ShortVarDec (ident:[]) (expr:exprList)) i =
    concat
      [ "var "
      , code ident i
      , " = "
      , code expr i
      ]
  code (ShortVarDec (ident:identList) (expr:exprList)) i =
    concat
      [ "var "
      , code ident i
      , " = "
      , code expr i
      , ", "
      , code (Assign identList exprList) i
      ]
  code (EmptyStmt) i = ""

instance Codeable IfStmt where
  code (IfStmt st expr stList (IfStmtCont Nothing)) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList (inject stList st) (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt st expr stList (IfStmtCont (Just (Right elseStmt)))) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList (inject stList st) (i + 1)
      , spacePrint i
      , "} else {\n"
      , codeList (inject stList st) (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt st expr stList (IfStmtCont (Just (Left ifStmt)))) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList (inject stList st) (i + 1)
      , spacePrint i
      , "} else "
      , code ifStmt i
      ]

instance Codeable Clause where
  code (Case exList stList) i =
    concat
      [ spacePrint i
      , "case "
      , commaSepList exList i
      , ":\n"
      , codeList stList (i + 1)
      , spacePrint (i+1)
      , "break;\n"
      ]
  code (Default stList) i =
    concat [spacePrint i, "default:\n", codeList stList (i + 1)]

class Injectable a where
  inject :: [a] -> SimpleStmt -> [a]

instance Injectable Clause where
  inject [] stmt = []
  inject ((Case exprs stmtList):clauses) stmt = (Case exprs (inject stmtList stmt)):(inject clauses stmt)
  inject ((Default stmtList): clauses) stmt = (Default (inject stmtList stmt)):(inject clauses stmt)

injectIf :: IfStmt -> SimpleStmt -> IfStmt
injectIf (IfStmt st expr stList (IfStmtCont Nothing)) stmt =
       (IfStmt st expr (inject stList stmt) (IfStmtCont Nothing))
injectIf (IfStmt st expr stList (IfStmtCont (Just (Right elseStmt)))) stmt =
       (IfStmt st expr (inject stList stmt) (IfStmtCont (Just (Right (inject elseStmt stmt)))))
injectIf (IfStmt st expr stList (IfStmtCont (Just (Left ifStmt)))) stmt =
       (IfStmt st expr (inject stList stmt) (IfStmtCont (Just (Left (injectIf ifStmt stmt)))))

instance Injectable Stmt where
  inject stmts EmptyStmt = stmts
  inject stmts stmt = (SimpleStmt stmt):stmts
