{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGenerator
  ( Codeable(..)
  ) where


import Data.List(intercalate)
import Data.Char(chr)
import Language


class Codeable a where
  code :: a -> Integer -> String

  codeList :: [a] -> Integer -> String
  codeList ps i = concatMap (`code` i) ps

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

emptyTypeValue (Array _ i) _ _ = concat ["new Array(", code i 0, ")"]
emptyTypeValue (Slice _) _ _ = "new Array()"
emptyTypeValue (Struct struct) i s =
  concat
    ["{};\n"
    , structPrint struct (i + 1) s]

--
commaSepList :: Codeable a => [a] -> Integer -> String
commaSepList string i = intercalate ", " (map (`code` i) string)

commaSpaceSepList :: Codeable a => [a] -> Integer -> String
commaSpaceSepList string i = intercalate ", \" \", " (map (`code` i) string)

varList :: [Identifier] -> (Maybe Type) -> [Expression] -> Integer -> String
varList [] exprs _ _ = ""
varList (var:[]) (Just t) [] i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , emptyTypeValue t i ""
          , ";\n"]
varList (var:vars) (Just t) [] i =
      concat
          [spacePrint i
          , ", "
          , code var i
          , " = "
          , emptyTypeValue t i ""
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

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: Codeable a => [a] -> Integer -> String
wrapSquareList xs i = concatMap wrapSquare (map (`code` i) xs)

--
dotSepList :: [String] -> String
dotSepList = intercalate "."

-- Need to do
instance Codeable Program where
  code (Program package alls) _ =
    concat
        [ "function append(list, addition) {\n"
        , "\tlist.push(addition);\n"
        , "\treturn list;\n"
        , "}\n\n"
        , codeList alls 0
        , "\n\nmain();\n"]

instance Codeable All where
  code (TopDec dec) _ = concat [code dec 0, "\n"]
  code (Function name params _ stmts) _ =
    concat
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
      , emptyTypeValue t i ""
      , ";\n"
      ]
  code (Variable (var:vars) (Just t) []) i =
    concat
      [spacePrint i
      , "var "
      , code var i
      , " = "
      , emptyTypeValue t i ""
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
      , emptyTypeValue (Alias s2) i ""
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

instance Codeable Identifier where
  code (IdOrType s) i = s
  code (IdArray s xs) i = concat [s, wrapSquareList (map (`code` 0) xs) i]
  code (IdField xs) i = intercalate "." $ map (`code` i) xs

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
  code (Print expr) i =
    concat [spacePrint i, "console.log(", commaSepList expr i, ");\n"]
  code (Println expr) i =
    concat [spacePrint i, "console.log(", commaSpaceSepList expr i, ", \"\\n\");\n"]
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
      , "for "
      , code simp1 0
      , "; ("
      , code expr 0
      , "); "
      , code simp2 0
      , " {\n"
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

instance Codeable FunctionCall where
  code (FunctionCall ident exprList) i =
    concat [code ident i, "(", commaSepList exprList i, ")"]

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

instance Codeable Expression where
  code (Brack expr) i = concat
    [ "("
    , code expr i
    , ")"
    ]
  code (Id ident) i = code ident i
  code (Literal lit) i = code lit i
  code (Unary op expr) i = concat [code op i, code expr i]
  code (Binary op expr1 expr2) i = concat
    [ code expr1 i
    , " "
    , code op i
    , code expr2 i
    ]
  code (ExprFuncCall func) i = code func i
  code (Append ident expr) i =
    concat ["append(", code ident i, ", ", code expr i, ")"]

-- Need to do
instance Codeable UnaryOp where
  code Pos _ = "+"
  code Neg _ = "-"
  code BoolNot _ = "!"
  code BitComplement _ = "~"

-- Need to do
instance Codeable BinaryOp where
  code Or _ = "|| "
  code And _ = "&& "
  code Equals _ = "== "
  code NotEquals _ = "!= "
  code LThan _ = "< "
  code LEThan _ = "<= "
  code GThan _ = "> "
  code GEThan _ = ">= "
  code Add _ = "+ "
  code Sub _ = "- "
  code Mult _ = "* "
  code Div _ = "/ "
  code Mod _ = "% "
  code BitAnd _ = "& "
  code BitOr _ = "| "
  code BitXor _ = "^ "
  code BitLShift _ = "<< "
  code BitRShift _ = ">> "
  code BitClear _ = "& ~"

-- Need to do
instance Codeable BinaryOpEq where
  code PlusEq _ = "+= "
  code MinusEq _ = "-= "
  code MulEq _ = "*= "
  code DivEq _ = "/= "
  code ModEq _ = "%= "
  code BitAndEq _ = "&= "
  code BitOrEq _ = "|= "
  code BitXorEq _ = "^= "
  code BitLShiftEq _ = "<<= "
  code BitRShiftEq _ = ">>= "
  code BitClearEq _ = "&= ~"

instance Codeable (Maybe Expression) where
  code Nothing i = "true"
  code (Just e) i = code e i

instance Codeable String where
  code s _ = s

instance Codeable Integer where
  code int _ = (show int)

instance Codeable Int where
  code int _ = (show int)

instance Codeable Literal where
  code (Int' i) _ = show i
  code (Float64 f) _ = show f
  code (Rune i) _ = (show . chr . fromIntegral) i
  code (String s) _ = s
  code (Raw s) _ = s

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
  inject stmts stmt = (SimpleStmt stmt):stmts
