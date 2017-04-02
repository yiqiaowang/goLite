{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGenerator
  ( Codeable(..)
  , codeProgram
  ) where


import Data.List(intercalate)
import Data.Char(chr)
import Language
import SymbolTable
import CodeGen.Native.Comparison
import CodeGen.Native.Array
import CodeGen.Expressions
import CodeGen.Codeable
import Data.Ix

--
nextContext :: History -> History
nextContext [] = undefined
nextContext (_ : t) = t

-- Takes a struct entry, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
structList :: ([Identifier], Type) -> Integer -> String -> Integer -> History -> String
structList ([], _) _ _ _ _ = ""
structList ((ident:idents), (Struct t)) i s index h =
  concat
    [spacePrint i
    , s
    , "."
    , code ident i h
    , " = "
    , emptyTypeValue (Struct t) i (concat [s, ".", code ident i h]) index h
    , structList (idents, (Struct t)) i s index h
    ]
structList ((ident:idents), t) i s index h =
  concat
    [spacePrint i
    , s
    , "."
    , code ident i h
    , " = "
    , emptyTypeValue t i (concat [s, ".", code ident i h]) index h
    , ";\n"
    , structList (idents, t) i s index h
    ]

-- Takes a struct, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
structPrint :: [([Identifier], Type)] -> Integer -> String -> Integer -> History -> String
structPrint [] _ _ _ _ = ""
structPrint (struct:structs) i s index h = concat
  [ structList struct i s index h
  , structPrint structs i s index h
  ]

-- Takes a type, the number of spaces to print,
-- the prefix (ex: "pt.x[1]"), and the index number for
-- multi-dimensional arrays
emptyTypeValue :: Type -> Integer -> String -> Integer -> History -> String
emptyTypeValue (Alias "string") _ _ _ _ = "\"\""
emptyTypeValue (Alias "bool") _ _ _ _ = "false"
emptyTypeValue (Alias "int") _ _ _ _ = "0"
emptyTypeValue (Alias "float64") _ _ _ _ = "0.0"
emptyTypeValue (Alias "rune") _ _ _ _ = "\'\\0\'"
emptyTypeValue (Alias str) _ _ _ _ = concat [str, "()"]
emptyTypeValue (Array t num) i s index h = concat
                         ["new Array("
                         , code num 0 h
                         , ");\n"
                         , spacePrint i
                         , "for (let GO_COUNT_"
                         , code index 0 h
                         , " = 0; GO_COUNT_"
                         , code index 0 h
                         , " < "
                         , code num 0 h
                         , "; GO_COUNT_"
                         , code index 0 h
                         , "++) {\n"
                         , spacePrint (i+1)
                         , code s 0 h
                         , "[GO_COUNT_"
                         , code index 0 h
                         , "] = "
                         , emptyTypeValue t (i+1) (concat[s, "[GO_COUNT_", code index 0 h, "]"]) (index + 1) h
                         , ";\n"
                         , spacePrint i
                         , "}"]
emptyTypeValue (Slice _) _ _ _ _ = "new Array()"
emptyTypeValue (Struct struct) i s index h =
  concat
    ["{};\n"
    , structPrint struct (i + 1) s index h]

tempVar :: Integer -> Integer -> History -> String
tempVar i indent hist = code (concat [ "GO_LITE_TEMP_" , code i 0 hist]) indent hist

--
temparize :: [Identifier] -> [Expression] -> Integer -> Integer -> History -> String
temparize [] _ _ _ _ = ""
temparize (ident:idents) (expr:exprs) i indent hist = concat [
                spacePrint (case i of 
                                0 -> 0
                                _ -> indent)
                , temparizeOne ident expr i hist
                , temparize idents exprs (i + 1) indent hist ]

temparizeOne :: Identifier -> Expression -> Integer -> History -> String
temparizeOne ident expr i hist = concat [
              "var "
              , tempVar i 0 hist
              , " = GO_LITE_COPY("
              , code expr 0 hist
              , ");\n" ]

codeProgram :: Program -> Integer -> [History] -> String
codeProgram (Program package alls) _ h = concat
  [ goLiteAppend
  , goLiteReadIndex
  , goLiteCopy
  , goLiteEquals
  , goLiteNotEquals
  , goLiteBoundsCheck
  , codeAlls alls 0 0 h
  ]

codeAlls :: [All] -> Int -> Integer -> [History] -> String
codeAlls [] _ _ _ = ""
codeAlls (TopDec dec:as) offset i h = code (TopDec dec) i (head h) ++ codeAlls as offset i h
codeAlls (Function a b c d:as) offset i h = code (Function a b c d) i (h!!(((length h)-1)-offset)) ++ codeAlls as (offset+1) i h

-- TODO not used anymore
-- Need to do
-- instance Codeable Program where
--   code (Program package alls) _ h = concat
--     [ goLiteAppend
--     , goLiteReadIndex
--     , goLiteCopy
--     , goLiteEquals
--     , goLiteNotEquals
--     , goLiteBoundsCheck
--     , codeList alls 0 h
--     ]

instance Codeable All where
  code (TopDec dec) _ h = concat [code dec 0 h, "\n"]
  code (Function name params _ stmts) _ h =
    case name of
      "main" -> concat
            [ "function "
            , code name 0 h
            , "("
            , commaSepList params 0 h
            , ") {\n"
            , codeList stmts 1 (nextContext h)
            , "}\nmain();\n\n"
            ]
      _ -> concat
            [ "function "
            , code name 0 h
            , "("
            , commaSepList params 0 h
            , ") {\n"
            , codeList stmts 1 (nextContext h)
            , "}\n\n"
            ]

instance Codeable TopLevel where
  code (VarDec var) i h = code var i h
  code (VarDecList vList) i h = codeList vList i h
  code (TypeDec t) i h = code t i h
  code (TypeDecList tList) i h = codeList tList i h

instance Codeable Variable where
  code (Variable [] _ []) _ h = ""
  code (Variable [var] (Just t) []) i h =
    concat
      [spacePrint i
      , "let "
      , code var i h
      , " = "
      , emptyTypeValue t i (code var i h) 0 h
      , ";\n"
      ]
  code (Variable (var:vars) (Just t) []) i h =
    concat
      [spacePrint i
      , "let "
      , code var i h
      , " = "
      , emptyTypeValue t i (code var i h) 0 h
      , ";\n"
      , code (Variable vars (Just t) []) i h
      ]
  code (Variable [var] _ (expr:exprs)) i h =
    concat
      [spacePrint i
      , "let "
      , code var i h
      , " = GO_LITE_COPY("
      , code expr i h
      , ");\n"
      ]
  code (Variable (var:vars) t (expr:exprs)) i h =
    concat
      [ spacePrint i
      , "let "
      , code var i h
      , " = GO_LITE_COPY("
      , code expr i h
      , ");\n"
      , code (Variable vars t exprs) i h
      ]

instance Codeable TypeName where
  code (TypeName (Alias s1) (Alias s2)) i h =
    concat
      [spacePrint i
      , "let "
      , code (Alias s1) i h
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return "
      , emptyTypeValue (Alias s2) i (code (Alias s1) i h) 0 h
      , ";\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Array _ num)) i h =
    concat
      [spacePrint i
      , "let "
      , code (Alias s) i h
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return new Array("
      , code num i h
      , ");\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Slice _)) i h =
    concat
      [spacePrint i
      , "let "
      , code (Alias s) i h
      , " = function() {\n"
      , spacePrint (i + 1)
      , "return new Array();\n"
      , spacePrint i
      , "}\n"]
  code (TypeName (Alias s) (Struct struct)) i h =
    concat
      [spacePrint i
      , "let "
      , code (Alias s) i h
      , " = function() {\n"
      , spacePrint (i + 1)
      , "let struct = {};\n"
      , structPrint struct (i + 1) "struct" 0 h
      , spacePrint (i + 1)
      , "return struct;\n"
      , spacePrint i
      , "}\n"]
  code (TypeName _ _) _ _ = ""

-- UNUSED!!!!!!!!!!!
instance Codeable Type where
  code (Alias s) _ _ = s
  code (Array t expr) _ h = concat ["new Array(", code expr 0 h, ")"]
  code (Slice t) _ _ = "new Array()"
  code (Struct list) _ _= "{}"

instance Codeable Parameter where
  code (Parameter idList _) i = commaSepList idList i

instance Codeable Stmt where
  code (StmtDec dec) i h = code dec i h
  code (SimpleStmt simp) i h = concat [spacePrint i, code simp i h, ";\n"]
  code (Print expr) i h = concat
    [spacePrint i, "process.stdout.write(JSON.stringify(", commaSepList expr i h, "));\n"]
  code (Println expr) i h = concat
    [spacePrint i, "console.log(", commaSepList expr i h, ");\n"]
  code (Return Nothing) i h = concat [spacePrint i, "return;\n"]
  code (Return (Just expr)) i h =
    concat [spacePrint i, "return ", code expr 0 h, ";\n"]
  code (If ifstmt) i h = concat [spacePrint i, code ifstmt i h]
  code (Switch EmptyStmt expr clauses) i h =
    concat
      [ spacePrint i
      , "switch ("
      , code expr 0 (nextContext h)
      , ") {\n"
      , codeList clauses (i + 1) (nextContext h)
      , spacePrint i
      , "}\n"
      ]
  code (Switch stmt expr clauses) i h =
    concat
      [ spacePrint i
      , "{\n"
      , code stmt (i + 1) (nextContext h)
      , spacePrint (i + 1)
      , "switch ("
      , code expr 0 (nextContext h)
      , ") {\n"
      , codeList clauses (i + 2) (nextContext h)
      , spacePrint (i + 1)
      , "}\n"
      , spacePrint i
      , "}\n"
      ]
  code (Infinite stmts) i h =
    concat
      [spacePrint i, "while (true) {\n", codeList stmts (i + 1) (nextContext h), spacePrint i, "}\n"]
  code (While expr stmts) i h =
    concat
      [ spacePrint i
      , "while ("
      , code expr 0 h
      , ") {\n"
      , codeList stmts (i + 1) (nextContext h)
      , spacePrint i
      , "}\n"
      ]
  code (For simp1 expr simp2 stmts) i h =
    concat
      [ spacePrint i
      , "for ("
      , code simp1 0 (nextContext h)
      , "; "
      , code expr 0 (nextContext h)
      , "; "
      , code simp2 0 (nextContext h)
      , ") {\n"
      , codeList stmts (i + 1) (nextContext $ nextContext h)
      , spacePrint i
      , "}\n"
      ]
  code (Block xs) i h = concat [spacePrint i, "{\n", codeList xs (i+1) (nextContext h), spacePrint i, "}\n"]
  code Break i h = concat [spacePrint i, "break;\n"]
  code Continue i h = concat [spacePrint i, "continue;\n"]

instance Codeable SimpleStmt where
  code (StmtFuncCall func) i h = code func i h
  code (Incr ident) i h =
    let incrementCode = concat [codeLHS ident i h, "++"] in
    if isIdArray ident
      then boundsCheck ident i h ++ incrementCode
      else incrementCode
  code (Decr ident) i h =
    let decrementCode = concat [codeLHS ident i h, "--"] in
    if isIdArray ident
      then boundsCheck ident i h ++ decrementCode
      else decrementCode
  code (ShortBinary opEq ident expr) i h =
    let opeqCode = concat [codeLHS ident i h, code opEq i h, code expr i h] in
    if isIdArray ident
      then boundsCheck ident i h ++ opeqCode
      else opeqCode
  code (Assign ids exps) indent h =
    let pairs = zip ids (range (0, (toInteger (length exps)))) in
      let arrays = filter isIdArray ids in concat
        [ concatMap (\array -> boundsCheck array indent h) arrays
        , temparize ids exps 0 indent h
        , intercalate ", " $ map (uncurry assign') pairs
        ]
    where
      assign' i e = concat
        [ codeLHS i indent h
        , " = "
        , tempVar e 0 h
        ]
  code (ShortVarDec ids exps) indent h =
    let pairs = zip ids (range (0, (toInteger (length exps)))) in concat
          [ temparize ids exps 0 indent h
          , "let " ++ intercalate ", " (map (uncurry svd') pairs) ]
    where
      svd' i e = concat
        [ codeLHS i indent h
        , " = "
        , tempVar e 0 h
        ]
  code EmptyStmt _ _ = ""

--
isIdArray (IdArray _ _) = True
isIdArray _ = False

--
boundsCheck (IdArray name indices) ident h =
  let depths = [0 .. length indices - 1] in
    concatMap (\depth -> check' name depth indices) depths
  where
    check' name depth exps =
      if depth == 0
        then "GO_LITE_BOUNDS_CHECK(" ++ name ++ ", " ++ code (head exps) 0 h ++ ");"
        else
          "GO_LITE_BOUNDS_CHECK(" ++ name ++ concatMap (wrapSquare . (\e -> code e ident h)) (take depth exps) ++ ", " ++ code (exps !! depth) 0 h ++ ");\n"

instance Codeable IfStmt where
  code (IfStmt EmptyStmt expr stList (IfStmtCont Nothing)) i h =
    concat
      [ "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 1) h
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt EmptyStmt expr stList (IfStmtCont (Just (Right elseStmt)))) i h =
    concat
      [ "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 1) h
      , spacePrint i
      , "} else {\n"
      , codeList elseStmt (i + 1) h
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt EmptyStmt expr stList (IfStmtCont (Just (Left ifStmt)))) i h =
    concat
      [ "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 1) h
      , spacePrint i
      , "} else "
      , code ifStmt i h
      ]
  code (IfStmt stmt expr stList (IfStmtCont Nothing)) i  h=
    concat
      [ "{\n"
      , spacePrint (i + 1)
      , code stmt (i + 1) h
      , ";\n"
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 2) h
      , spacePrint (i + 1)
      , "}\n"
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt stmt expr stList (IfStmtCont (Just (Right elseStmt)))) i h =
    concat
      [ "{\n"
      , spacePrint (i + 1)
      , code stmt (i + 1) h
      , ";\n"
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 2) h
      , spacePrint (i + 1)
      , "} else {\n"
      , codeList elseStmt (i + 2) h
      , spacePrint (i + 1)
      , "}\n"
      , spacePrint i
      , "}\n"
      ]
  code (IfStmt stmt expr stList (IfStmtCont (Just (Left ifStmt)))) i h =
    concat
      [ "{\n"
      , code stmt (i + 1) h
      , spacePrint (i + 1)
      ,  "if ("
      , code expr 0 h
      , ") {\n"
      , codeList stList (i + 2) h
      , spacePrint (i + 1)
      , "} else"
      , code ifStmt (i + 1) h
      , spacePrint i
      , "}\n"
      ]

instance Codeable Clause where
  code (Case exList stList) i h =
    concat
      [ spacePrint i
      , "case "
      , commaSepList exList i h
      , ":\n"
      , codeList stList (i + 1) h
      , spacePrint (i+1)
      , "break;\n"
      ]
  code (Default stList) i h =
    concat [spacePrint i, "default:\n", codeList stList (i + 1) h]

-- TODO use for ids on left hand side of assignments
codeLHS (IdOrType s) _ _= s
codeLHS (IdArray s xs) i h = concat [s, wrapSquareList (map (\x -> code x 0 h) xs) i h]
codeLHS (IdField xs) i h = intercalate "." $ map (\x -> code x i h) xs
