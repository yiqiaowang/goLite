{-# LANGUAGE FlexibleInstances #-}

module Pretty.TypedPretty
  ( typedPretty
  , prettyPrintProgram
  ) where


import Data.Char (chr)
import Data.List (intercalate)
import Language
import SymbolTable
import Pretty.Pretty
import TypeChecker


class TypedPretty a where
  typedPretty :: a -> Integer -> History -> String

  prettyList :: [a] -> Integer -> History -> String
  prettyList ps i h = concatMap (\p -> typedPretty p i h) ps

  prettyList' :: [a] -> Integer -> History -> String
  prettyList' [] _ _ = ""
  prettyList' (p:ps) i h = typedPretty p i h ++ prettyList' ps i (nextContext h)

--
commentType :: TypeCheckable a => a -> History -> String
commentType a [] = undefined
commentType a (ctxt : _) =
  case typeCheck (SymbolTable ctxt [] []) a of
    Right (Just type', _) -> "/*" ++ show type' ++ "*/"
    Right (Nothing, _) -> error $ "no type info"
    Left e -> error $ show e

--
nextContext :: History -> History
nextContext [] = undefined
nextContext (_ : t) = t

--
commaSepList :: TypedPretty a => [a] -> Integer -> History -> String
commaSepList string i h = intercalate ", " (map (\p -> typedPretty p i h) string)

--
structList :: ([Identifier], Type) -> Integer -> History -> String
structList (idList, t) i h = concat
  [ spacePrint i
  , commaSepList idList i h
  , " "
  , typedPretty t i h
  , ";\n"
  ]

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: TypedPretty a => [a] -> Integer -> History -> String
wrapSquareList ps i h = concatMap wrapSquare (map (\p -> typedPretty p i h) ps)

--
dotSepList :: [String] -> String
dotSepList = intercalate "."


prettyPrintProgram :: Program -> Integer -> [History] -> String

prettyPrintProgram (Program package alls) _ h = concat
    [ "package "
    , package
    , ";\n"
    , "\n"
    , prettyPrintAlls alls 0 0 h
    ]

prettyPrintAlls :: [All] -> Int -> Integer -> [History] -> String
prettyPrintAlls [] _ _ _ = ""
prettyPrintAlls (TopDec dec:as) offset i h = typedPretty (TopDec dec) i (head h) ++ prettyPrintAlls as offset i h
prettyPrintAlls (Function a b c d:as) offset i h = typedPretty (Function a b c d) i (h!!(((length h)-1)-offset)) ++ prettyPrintAlls as (offset+1) i h

instance TypedPretty All where
  typedPretty (TopDec dec) _ h = typedPretty dec 0 h
  typedPretty (Function name params Nothing stmts) _ h = concat
    [ "func "
    , typedPretty name 0 h
    , "("
    , commaSepList params 0 ctxt
    , ") {\n"
    , prettyList stmts 1 ctxt
    , "}\n\n"
    ] where ctxt = if length h <= 1 then h else (nextContext h)
                     
  typedPretty (Function name params (Just type') stmts) _ h = concat
    [ "func "
    , typedPretty name 0 h
    , "("
    , commaSepList params 0 ctxt
    , ") "
    , typedPretty type' 0 ctxt
    , " {\n"
    , prettyList stmts 1 ctxt
    , "}\n\n"
    ] where ctxt = if length h <= 1 then h else (nextContext h)





instance TypedPretty TopLevel where
  typedPretty (VarDec (Variable var (Just t) [])) i h = concat
    [ spacePrint i
    , "var "
    , commaSepList var i h
    , " "
    , typedPretty t i h
    , ";\n"
    ]
  typedPretty (VarDec (Variable var (Just t) expr)) i h = concat
    [ spacePrint i
    , "var "
    , commaSepList var i h
    , " "
    , typedPretty t i h
    , " = "
    , commaSepList expr i h
    , ";\n"
    ]
  typedPretty (VarDec (Variable var Nothing expr)) i h = concat
    [ spacePrint i
    , "var "
    , commaSepList var i h
    , " = "
    , commaSepList expr i h
    , ";\n"
    ]
  typedPretty (VarDecList vList) i h = concat
    [ spacePrint i
    , "var (\n"
    , prettyList vList (i + 1) h
    , spacePrint i
    , ");\n"
    ]
  typedPretty (TypeDec (TypeName ident t)) i h = concat
    [ spacePrint i
    , "type "
    , typedPretty ident i h
    , " "
    , typedPretty t i h
    , ";\n"
    ]
  typedPretty (TypeDecList tList) i h = concat
    [ spacePrint i
    , "type (\n"
    , prettyList tList (i + 1) h
    , spacePrint i
    , ");\n"
    ]

instance TypedPretty Variable where
  typedPretty (Variable var (Just t) []) i h = concat
    [ spacePrint i
    , commaSepList var i h
    , " "
    , typedPretty t i h
    , ";\n"
    ]
  typedPretty (Variable var (Just t) expr) i h = concat
    [ spacePrint i
    , commaSepList var i h
    , " "
    , typedPretty t i h
    , " = "
    , commaSepList expr i h
    , ";\n"
    ]
  typedPretty (Variable var Nothing expr) i h = concat
    [ spacePrint i
    , commaSepList var i h
    , " = "
    , commaSepList expr i h
    , ";\n"
    ]

instance TypedPretty TypeName where
  typedPretty (TypeName ident t) i h = concat
    [ spacePrint i
    , typedPretty ident i h
    , " "
    , typedPretty t i h
    , ";\n"
    ]

instance TypedPretty Identifier where
  typedPretty (IdOrType s) i h = s
  typedPretty (IdArray s xs) i h =
    s ++ wrapSquareList (map (\p -> typedPretty p i h) xs) i h
  typedPretty (IdField xs) i h = intercalate "." $ map (\p -> typedPretty p i h) xs

instance TypedPretty Type where
  typedPretty (Alias s) _ _ = s
  typedPretty (Array t expr) _ h = concat
    [ "["
    , typedPretty expr 0 h
    , "]"
    , typedPretty t 0 h
    ]
  typedPretty (Slice t) _ h = concat
    [ "[]"
    , typedPretty t 0 h
    ]
  typedPretty (Struct list) i h = concat
    [ "struct {\n"
    , concatMap (\p -> structList p (i + 1) h) list
    , "}"
    ]

instance TypedPretty Parameter where
  typedPretty (Parameter idList t) i h = concat
    [ commaSepList idList i h
    , " "
    , typedPretty t 0 h
    ]

instance TypedPretty Stmt where
  typedPretty (StmtDec dec) i h = typedPretty dec i h
  typedPretty (SimpleStmt simp) i h = concat
    [ spacePrint i
    , typedPretty simp i h
    , ";\n"
    ]
  typedPretty (Print expr) i h = concat
    [ spacePrint i
    , "print("
    , commaSepList expr i h
    , ");\n"
    ]
  typedPretty (Println expr) i h = concat
    [ spacePrint i
    , "println("
    , commaSepList expr i h
    , ");\n"
    ]
  typedPretty (Return Nothing) i h = concat [spacePrint i, "return;\n"]
  typedPretty (Return (Just expr)) i h = concat
    [ spacePrint i
    , "return "
    , typedPretty expr 0 h
    , ";\n"
    ]
  typedPretty (If ifstmt) i h = concat
    [ spacePrint i
    , typedPretty ifstmt i (nextContext h)
    ]
  typedPretty (Switch stmt Nothing c) i h = concat
    [ spacePrint i
    , "switch "
    , typedPretty stmt 0 (nextContext h)
    , "; {\n"
    , prettyListClause c (i + 1) (nextContext h)
    , spacePrint i
    , "}\n"
    ]
  typedPretty (Switch stmt (Just expr) c) i h = concat
    [ spacePrint i
    , "switch "
    , typedPretty stmt 0 (nextContext h)
    , "; "
    , typedPretty expr 0 (nextContext h)
    , " {\n"
    , prettyListClause c (i + 1) (nextContext h)
    , spacePrint i
    , "}\n"
    ]
  typedPretty (Infinite stmts) i h = concat
    [ spacePrint i
    , "for {\n"
    , prettyList stmts (i + 1) (nextContext h)
    , spacePrint i
    , "}\n"
    ]
  typedPretty (While expr stmts) i h = concat
    [ spacePrint i
    , "for "
    , typedPretty expr 0 h
    , " {\n"
    , prettyList stmts (i + 1) (nextContext h)
    , spacePrint i
    , "}\n"
    ]
  typedPretty (For simp1 expr simp2 stmts) i h = concat
    [ spacePrint i
    , "for "
    , typedPretty simp1 0 (nextContext h)
    , "; "
    , typedPretty expr 0 (nextContext h)
    , "; "
    , typedPretty simp2 0 (nextContext h)
    , " {\n"
    , prettyList stmts (i + 1) (nextContext $ nextContext h)
    , spacePrint i
    , "}\n"
    ]
  typedPretty (Block xs) i h = concat
    [ spacePrint i
    , "{\n"
    , prettyList xs (i+1) (nextContext h)
    , spacePrint i
    , "};\n"
    ]
  typedPretty Break i h = concat [spacePrint i, "break;\n"]
  typedPretty Continue i h = concat [spacePrint i, "continue;\n"]

instance TypedPretty SimpleStmt where
  typedPretty (StmtFuncCall func) i h = (typedPretty func i h)
  typedPretty (Incr ident) i h = concat [typedPretty ident i h, "++"]
  typedPretty (Decr ident) i h = concat [typedPretty ident i h, "--"]
  typedPretty (Assign idList exprList) i h = concat
    [ commaSepList idList i h
    , " = "
    , commaSepList exprList i h
    ]
  typedPretty (ShortBinary opEq ident expr) i h = concat
    [ typedPretty ident i h
    , " "
    , pretty opEq i
    , " "
    , typedPretty expr i h
    ]
  typedPretty (ShortVarDec idList exprList) i h = concat
    [ commaSepList idList i h
    , " := "
    , commaSepList exprList i h
    ]
  typedPretty EmptyStmt i h = ""

instance TypedPretty FunctionCall where
  typedPretty (FunctionCall ident exprList) i h = concat
    [ typedPretty ident i h
    , "("
    , commaSepList exprList i h
    , ")"
    ]

instance TypedPretty IfStmt where
  typedPretty (IfStmt st expr stList (IfStmtCont Nothing)) i h = concat
    [ "if "
    , typedPretty st 0 h
    , "; "
    , typedPretty expr 0 h
    , " {\n"
    , prettyList stList (i + 1) h
    , spacePrint i
    , "}\n"
    ]
  typedPretty (IfStmt st expr stList (IfStmtCont (Just (Right elseStmt)))) i h =
    concat
      [ "if "
      , typedPretty st 0 h
      , "; "
      , typedPretty expr 0 h
      , " {\n"
      , prettyList stList (i + 1) h
      , spacePrint i
      , "} else {\n"
      , prettyList elseStmt (i + 1) h
      , spacePrint i
      , "}\n"
      ]
  typedPretty (IfStmt st expr stList (IfStmtCont (Just (Left ifStmt)))) i h =
    concat
      [ "if "
      , typedPretty st 0 h
      , "; "
      , typedPretty expr 0 h
      , " {\n"
      , prettyList stList (i + 1) h
      , spacePrint i
      , "} else "
      , typedPretty ifStmt i h
      ]



prettyListClause :: [Clause] -> Integer -> History -> String
prettyListClause [] _ _ = ""
prettyListClause (p:ps) i h = typedPretty p i (nextContext h) ++ prettyListClause ps i (nextContext h)


 

instance TypedPretty Clause where
  typedPretty (Case exList stList) i h = concat
    [ spacePrint i
    , "case "
    , commaSepList exList i h
    , ":\n"
    , prettyList stList (i + 1) h
    ]
  typedPretty (Default stList) i h = concat
    [ spacePrint i
    , "default:\n"
    , prettyList stList (i + 1) h
    ]

instance TypedPretty Expression where
  typedPretty (Brack expr) i h = concat
    [ "("
    , typedPretty expr i h
    , ")"
    ]
  typedPretty (Id ident) i h = typedPretty ident i h ++ commentType ident h
  typedPretty (Literal lit) i h = typedPretty lit i h
  typedPretty (Unary op expr) i h = concat
    [ pretty op i
    , typedPretty expr i h
    ]
  typedPretty e@(Binary op expr1 expr2) i h = concat
    [ typedPretty expr1 i h
    , " "
    , pretty op i
    , " "
    , typedPretty expr2 i h
    , commentType e h
    ]
  typedPretty f@(ExprFuncCall func) i h =
    typedPretty func i h ++ commentType f h
  typedPretty (Append ident expr) i h = concat
    [ "append("
    , typedPretty ident i h
    , ", "
    , typedPretty expr i h
    , ")"
    ]

instance TypedPretty Literal where
  typedPretty (Int' i) _ _ = show i ++ "/*int*/"
  typedPretty (Float64 f) _ _ = show f ++ "/*flaot64*/"
  typedPretty (Rune i) _ _ = (show . chr . fromIntegral) i ++ "/*rune*/"
  typedPretty (String s) _ _ = s ++ "/*string*/"
  typedPretty (Raw s) _ _ = s ++ "/*string*/"

instance TypedPretty (Maybe Expression) where
  typedPretty Nothing _ _ = ""
  typedPretty (Just e) i h = typedPretty e i h

instance TypedPretty String where
  typedPretty s _ _ = s

instance TypedPretty Integer where
  typedPretty i _ h = show i

instance TypedPretty Int where
  typedPretty i _ h = show i
