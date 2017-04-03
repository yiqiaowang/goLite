{-# LANGUAGE FlexibleInstances #-}
module CodeGen.Expressions where

import Data.List(intercalate)
import Data.Char(chr)
import CodeGen.Codeable
import Language
import SymbolTable


instance Codeable Expression where
  code (Brack expr) i h = concat
    [ "("
    , code expr i h
    , ")"
    ]
  code (Id ident) i h = code ident i h
  code (Literal lit) i h = code lit i h
  code (Unary op expr) i h = concat [code op i h, code expr i h]
  code (Binary Equals expr1 expr2) i h =
    "GO_LITE_EQUALS(" ++ code expr1 i h ++ ", " ++ code expr2 i h ++ ")"
  code (Binary NotEquals expr1 expr2) i h =
    "GO_LITE_NOT_EQUALS(" ++ code expr1 i h ++ ", " ++ code expr2 i h ++ ")"
  code e@(Binary Div expr1 expr2) i h = case (getType expr1 h, getType expr2 h) of
    (Alias "int", Alias "int") ->
      "GO_LITE_INT_DIV(" ++ code expr1 i h ++ ", " ++ code expr2 i h ++ ")"
    _ -> code e i h
  code (Binary op expr1 expr2) i h = concat
    [ code expr1 i h
    , " "
    , code op i h
    , code expr2 i h
    ]
  code (ExprFuncCall func) i h = code func i h
  code (Append ident expr) i h =
    concat ["GO_LITE_APPEND(", code ident i h, ", ", code expr i h, ")"]

instance Codeable FunctionCall where
  code (FunctionCall ident exprList) i h =
    concat [code ("u_" ++ ident) i h, "(", commaSepList (map (`copyWrap` h) exprList) 0 h, ")"]

-- Need to do
instance Codeable UnaryOp where
  code Pos _ h = "+"
  code Neg _ h = "-"
  code BoolNot _ h = "!"
  code BitComplement _ h = "~"

-- Need to do
instance Codeable BinaryOp where
  code Or _ _ = "|| "
  code And _ _ = "&& "
  code Equals _ _ = "== "
  code NotEquals _ _ = "!= "
  code LThan _ _ = "< "
  code LEThan _ _ = "<= "
  code GThan _ _ = "> "
  code GEThan _ _ = ">= "
  code Add _ _ = "+ "
  code Sub _ _ = "- "
  code Mult _ _ = "* "
  code Div _ _ = "/ "
  code Mod _ _ = "% "
  code BitAnd _ _ = "& "
  code BitOr _ _ = "| "
  code BitXor _ _ = "^ "
  code BitLShift _ _ = "<< "
  code BitRShift _ _ = ">> "
  code BitClear _ _ = "& ~"

-- Need to do
instance Codeable BinaryOpEq where
  code PlusEq _ _ = "+= "
  code MinusEq _ _ = "-= "
  code MulEq _ _ = "*= "
  code DivEq _ _ = "/= "
  code ModEq _ _ = "%= "
  code BitAndEq _ _ = "&= "
  code BitOrEq _ _ = "|= "
  code BitXorEq _ _ = "^= "
  code BitLShiftEq _ _ = "<<= "
  code BitRShiftEq _ _ = ">>= "
  code BitClearEq _ _ = "&= ~"

instance Codeable (Maybe Expression) where
  code Nothing i _ = "true"
  code (Just e) i h = code e i h

instance Codeable String where
  code s _ _ = s

instance Codeable Integer where
  code int _ _ = show int

instance Codeable Int where
  code int _ _ = show int

instance Codeable Literal where
  code (Int' i) _ _ = show i
  code (Float64 f) _ _ = show f
  code (Rune i) _ _ = show i
  code (String s) _ _ = s
  code (Raw s) _ _ = s

instance Codeable Identifier where
  code (IdOrType "true") _ _ = "true"
  code (IdOrType "false") _ _ = "false"
  code (IdOrType s) _ _ = "u_" ++ s
  code (IdArray s xs) i h = code' s $ reverse xs
    where
      code' s [x] =
        "GO_LITE_READ_INDEX(u_" ++ s ++ ", " ++ code x i h ++ ")"
      code' s (x : xs') =
        "GO_LITE_READ_INDEX(u_" ++ code' s xs' ++ " ," ++ code x i h ++ ")"
  code (IdField xs) i h = intercalate "." $ map (\x -> code x i h) xs

--
copyWrap :: Expression -> History -> String
copyWrap expr h = concat["GO_LITE_COPY(", code expr 0 h, ")"]
