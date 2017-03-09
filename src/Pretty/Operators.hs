module Pretty.Operators where


import Pretty.Pretty
import Language.Operators


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
