{-# LANGUAGE FlexibleInstances #-}
module CodeGen.Expressions where

import Data.List(intercalate)
import Data.Char(chr)
import CodeGen.Codeable
import Language


instance Codeable Expression where
  code (Brack expr) i = concat
    [ "("
    , code expr i
    , ")"
    ]
  code (Id ident) i = code ident i
  code (Literal lit) i = code lit i
  code (Unary op expr) i = concat [code op i, code expr i]
  code (Binary Equals expr1 expr2) i =
    "GO_LITE_EQUALS(" ++ code expr1 i ++ ", " ++ code expr2 i ++ ")"
  code (Binary NotEquals expr1 expr2) i =
    "GO_LITE_NOT_EQUALS(" ++ code expr1 i ++ ", " ++ code expr2 i ++ ")"
  code (Binary op expr1 expr2) i = concat
    [ code expr1 i
    , " "
    , code op i
    , code expr2 i
    ]
  code (ExprFuncCall func) i = code func i
  code (Append ident expr) i =
    concat ["GO_LITE_APPEND(", code ident i, ", ", code expr i, ")"]

instance Codeable FunctionCall where
  code (FunctionCall ident exprList) i =
    concat [code ident i, "(", commaSepList (map copyWrap exprList) 0, ")"]

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

instance Codeable Identifier where
  code (IdOrType s) i = s
  code (IdArray s xs) i = concat [s, wrapSquareList (map (`code` 0) xs) i]
  code (IdField xs) i = intercalate "." $ map (`code` i) xs

--
copyWrap :: Expression -> String
copyWrap expr = concat["GO_LITE_COPY(", code expr 0, ")"]
