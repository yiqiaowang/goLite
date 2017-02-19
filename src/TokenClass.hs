module TokenClass
  ( TokenClass(..)
  , IntType(..)
  ) where


-- Each action has type :: String -> TokenClass -> Token
data TokenClass
  = TokenBreak
  | TokenCase
  | TokenChan
  | TokenConst
  | TokenContinue
  | TokenDefault
  | TokenDefer
  | TokenElse
  | TokenFallthrough
  | TokenFor
  | TokenFunc
  | TokenGo
  | TokenGoto
  | TokenIf
  | TokenImport
  | TokenInterface
  | TokenMap
  | TokenPackage
  | TokenRange
  | TokenReturn
  | TokenSelect
  | TokenStruct
  | TokenSwitch
  | TokenType
  | TokenVar
  | TokenPrint
  | TokenPrintln
  | TokenAppend
  | TokenAdd
  | TokenSub
  | TokenMult
  | TokenDiv
  | TokenMod
  | TokenBitAnd
  | TokenBitOr
  | TokenBitXor
  | TokenBitLShift
  | TokenBitRShift
  | TokenBitClear
  | TokenAddEq
  | TokenSubEq
  | TokenMultEq
  | TokenDivEq
  | TokenModEq
  | TokenBitAndEq
  | TokenBitOrEq
  | TokenBitXorEq
  | TokenBitLShiftEq
  | TokenBitRShiftEq
  | TokenBitClearEq
  | TokenLogAnd
  | TokenLogOr
  | TokenChannel
  | TokenInc
  | TokenDec
  | TokenBoolEq
  | TokenBoolLT
  | TokenBoolGT
  | TokenEq
  | TokenBoolNot
  | TokenBoolNotEq
  | TokenBoolLTE
  | TokenBoolGTE
  | TokenShortDec
  | TokenVariadic
  | TokenLParen
  | TokenRParen
  | TokenLSquare
  | TokenRSquare
  | TokenLCurly
  | TokenRCurly
  | TokenPeriod
  | TokenComma
  | TokenColon
  | TokenSemicolon
  | TokenId String
  | TokenFloatVal Float
  | TokenIntVal IntType Integer
  | TokenStringVal String
  | TokenRuneVal Integer
  | TokenRawVal String
  | TokenEOF
  deriving (Eq,Show)


data IntType
  = Decimal
  | Octal
  | Hex
  deriving (Eq, Show)
