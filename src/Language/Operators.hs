module Language.Operators where


data UnaryOp
  = Pos
  | Neg
  | BoolNot
  | BitComplement
  deriving (Eq, Ord, Show)


data BinaryOp
  = Or
  | And
  | Equals
  | NotEquals
  | LThan
  | LEThan
  | GThan
  | GEThan
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | BitLShift
  | BitRShift
  | BitClear
  deriving (Eq, Ord, Show)


data BinaryOpEq
  = PlusEq
  | MinusEq
  | MulEq
  | DivEq
  | ModEq
  | BitAndEq
  | BitOrEq
  | BitXorEq
  | BitLShiftEq
  | BitRShiftEq
  | BitClearEq
  deriving (Eq, Show)
