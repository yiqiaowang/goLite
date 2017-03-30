{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGenerator
  ( Codeable(..)
  ) where


import Data.List(intercalate)
import Data.Char(chr)
import Language
import CodeGen.Native.Comparison
import CodeGen.Native.Array
import CodeGen.Expressions
import CodeGen.Codeable

-- Takes a struct entry, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
structList :: ([Identifier], Type) -> Integer -> String -> Integer -> String
structList ([], _) _ _ _ = ""
structList ((ident:idents), (Struct t)) i s index =
  concat
    [spacePrint i
    , s
    , "."
    , code ident i
    , " = "
    , emptyTypeValue (Struct t) i (concat [s, ".", code ident i]) index
    , structList (idents, (Struct t)) i s index]
structList ((ident:idents), t) i s index =
  concat
    [spacePrint i
    , s
    , "."
    , code ident i
    , " = "
    , emptyTypeValue t i (concat [s, ".", code ident i]) index
    , ";\n"
    , structList (idents, t) i s index]

-- Takes a struct, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
structPrint :: [([Identifier], Type)] -> Integer -> String -> Integer -> String
structPrint [] _ _ _ = ""
structPrint (struct:structs) i s index = concat[structList struct i s index, structPrint structs i s index]

-- Takes a type, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
emptyTypeValue :: Type -> Integer -> String -> Integer -> String
emptyTypeValue (Alias "string") _ _ _ = "\"\""
emptyTypeValue (Alias "bool") _ _ _ = "false"
emptyTypeValue (Alias "int") _ _ _ = "0"
emptyTypeValue (Alias "float64") _ _ _ = "0.0"
emptyTypeValue (Alias "rune") _ _ _ = "\'\\0\'"
emptyTypeValue (Alias str) _ _ _ = concat[str, "()"]
emptyTypeValue (Array t num) i s index = concat
                         ["new Array("
                         , code num 0
                         , ");\n"
                         , spacePrint i
                         , "for (var GO_COUNT_"
                         , code index 0
                         , " = 0; GO_COUNT_"
                         , code index 0
                         , " < "
                         , code num 0
                         , "; GO_COUNT_"
                         , code index 0
                         , "++) {\n"
                         , spacePrint (i+1)
                         , code s 0
                         , "[GO_COUNT_"
                         , code index 0
                         , "] = "
                         , emptyTypeValue t (i+1) (concat[s, "[GO_COUNT_", code index 0, "]"]) (index + 1)
                         , ";\n"
                         , spacePrint i
                         , "}"]
emptyTypeValue (Slice _) _ _ _ = "new Array()"
emptyTypeValue (Struct struct) i s index =
  concat
    ["{};\n"
    , structPrint struct (i + 1) s index]

-- Need to do
instance Codeable Program where
  code (Program package alls) _ = concat
    [ goLiteAppend
    , goLiteReadIndex
    , goLiteCopy
    , goLiteEquals
    , goLiteNotEquals
    , goLiteBoundsCheck
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
      , emptyTypeValue t i (code var i) 0
      , ";\n"
      ]
  code (Variable (var:vars) (Just t) []) i =
    concat
      [spacePrint i
      , "var "
      , code var i
      , " = "
      , emptyTypeValue t i (code var i) 0
      , "\n"
      , code (Variable vars (Just t) []) i
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
      , code (Variable vars t exprs) i
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
      , emptyTypeValue (Alias s2) i (code (Alias s1) i) 0
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
      , structPrint struct (i + 1) s 0
      , spacePrint (i + 1)
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
    [spacePrint i, "console.log(", commaSepList expr i, ");\n"]
  code (Return Nothing) i = concat [spacePrint i, "return;\n"]
  code (Return (Just expr)) i =
    concat [spacePrint i, "return ", code expr 0, ";\n"]
  code (If ifstmt) i = concat [spacePrint i, code ifstmt i]
  code (Switch EmptyStmt expr clauses) i =
    concat
      [ spacePrint i
      , "switch ("
      , code expr 0
      , ") {\n"
      , codeList clauses (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (Switch stmt expr clauses) i =
    concat
      [ spacePrint i
      , "{\n"
      , code stmt (i + 1)
      , spacePrint (i + 1)
      , "switch ("
      , code expr 0
      , ") {\n"
      , codeList clauses (i + 2)
      , spacePrint (i + 1)
      , "}\n"
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
  code (Assign ids exps) ident =
    let pairs = zip ids exps in
      let arrays = filter isIdArray ids in concat
        [ concatMap (`boundsCheck` ident) arrays
        , intercalate ", " $ map (uncurry assign') pairs
        ]
    where
      assign' i e = concat
        [ code i ident
        , " = "
        , code e ident
        ]
      isIdArray (IdArray _ _ ) = True
      isIdArray _ = False
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

boundsCheck (IdArray s []) _ = ""
boundsCheck (IdArray s (x : xs)) ident =
  "GO_LITE_BOUNDS_CHECK(" ++ s ++ ", " ++ code x ident ++ ");"

instance Codeable IfStmt where
  code (IfStmt EmptyStmt expr stList (IfStmtCont Nothing)) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt EmptyStmt expr stList (IfStmtCont (Just (Right elseStmt)))) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 1)
      , spacePrint i
      , "} else {\n"
      , codeList elseStmt (i + 1)
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt EmptyStmt expr stList (IfStmtCont (Just (Left ifStmt)))) i =
    concat
      [ "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 1)
      , spacePrint i
      , "} else "
      , code ifStmt i
      ]
  code (IfStmt stmt expr stList (IfStmtCont Nothing)) i =
    concat
      [ "{\n"
      , code stmt (i + 1)
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 2)
      , spacePrint (i + 1)
      , "}\n"
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt stmt expr stList (IfStmtCont (Just (Right elseStmt)))) i =
    concat
      [ "{\n"
      , code stmt (i + 1)
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 2)
      , spacePrint (i + 1)
      , "} else {\n"
      , codeList elseStmt (i + 2)
      , spacePrint (i + 1)
      , "}\n"
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt stmt expr stList (IfStmtCont (Just (Left ifStmt)))) i =
    concat
      [ "{\n"
      , code stmt (i + 1)
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0
      , ") {\n"
      , codeList stList (i + 2)
      , spacePrint (i + 1)
      , "} else"
      , code ifStmt (i + 1)
      , spacePrint i
      , "}\n"
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