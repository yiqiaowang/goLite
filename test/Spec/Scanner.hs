module Spec.Scanner where


import Test.Hspec
import Scanner


spec :: Spec
spec =
  describe "Scanner" $
    it "corrextly scans:" $ do
      let scansAs string cls = tokenClass string `shouldBe` Right cls

      -- Go keywords
      "break" `scansAs` TokenBreak
      "case" `scansAs` TokenCase
      "chan" `scansAs` TokenChan
      "const" `scansAs` TokenConst
      "continue" `scansAs` TokenContinue
      "default" `scansAs` TokenDefault
      "defer" `scansAs` TokenDefer
      "else" `scansAs` TokenElse
      "fallthrough" `scansAs` TokenFallthrough
      "for" `scansAs` TokenFor
      "func" `scansAs` TokenFunc
      "go" `scansAs` TokenGo
      "goto" `scansAs` TokenGoto
      "if" `scansAs` TokenIf
      "import" `scansAs` TokenImport
      "interface" `scansAs` TokenInterface
      "map" `scansAs` TokenMap
      "package" `scansAs` TokenPackage
      "range" `scansAs` TokenRange
      "return" `scansAs` TokenReturn
      "select" `scansAs` TokenSelect
      "struct" `scansAs` TokenStruct
      "switch" `scansAs` TokenSwitch
      "type" `scansAs` TokenType
      "var" `scansAs` TokenVar

      -- goLite keywords
      "print" `scansAs` TokenPrint
      "println" `scansAs` TokenPrintln
      "append" `scansAs` TokenAppend

      -- operators
      "+" `scansAs` TokenAdd
      "-" `scansAs` TokenSub
      "*" `scansAs` TokenMult
      "/" `scansAs` TokenDiv
      "%" `scansAs` TokenMod

      "&" `scansAs` TokenBitAnd
      "|" `scansAs` TokenBitOr
      "^" `scansAs` TokenBitXor
      "<<" `scansAs` TokenBitLShift
      ">>" `scansAs` TokenBitRShift
      "&^" `scansAs` TokenBitClear

      "+=" `scansAs` TokenAddEq
      "-=" `scansAs` TokenSubEq
      "*=" `scansAs` TokenMultEq
      "/=" `scansAs` TokenDivEq
      "%=" `scansAs` TokenModEq

      "&=" `scansAs` TokenBitAndEq
      "|=" `scansAs` TokenBitOrEq
      "^=" `scansAs` TokenBitXorEq
      "<<=" `scansAs` TokenBitLShiftEq
      ">>=" `scansAs` TokenBitRShiftEq
      "&^=" `scansAs` TokenBitClearEq

      "&&" `scansAs` TokenLogAnd
      "||" `scansAs` TokenLogOr
      "<-" `scansAs` TokenChannel
      "++" `scansAs` TokenInc
      "--" `scansAs` TokenDec

      "==" `scansAs` TokenBoolEq
      "<" `scansAs` TokenBoolLT
      ">" `scansAs` TokenBoolGT
      "=" `scansAs` TokenEq
      "!" `scansAs` TokenBoolNot

      "!=" `scansAs` TokenBoolNotEq
      "<=" `scansAs` TokenBoolLTE
      ">=" `scansAs` TokenBoolGTE
      ":=" `scansAs` TokenShortDec
      "..." `scansAs` TokenVariadic

      "(" `scansAs` TokenLParen
      ")" `scansAs` TokenRParen
      "[" `scansAs` TokenLSquare
      "]" `scansAs` TokenRSquare
      "{" `scansAs` TokenLCurly
      "}" `scansAs` TokenRCurly
      "." `scansAs` TokenPeriod
      "," `scansAs` TokenComma
      ";" `scansAs` TokenSemicolon
      ":" `scansAs` TokenColon

      "0" `scansAs` TokenIntVal Decimal 0
      "100" `scansAs` TokenIntVal Decimal 100

      "01" `scansAs` TokenIntVal Octal 1
      "0377" `scansAs` TokenIntVal Octal 255
      "032457" `scansAs` TokenIntVal Octal 13615

      "0x0" `scansAs` TokenIntVal Hex 0
      "0x1" `scansAs` TokenIntVal Hex 1
      "0xFF" `scansAs` TokenIntVal Hex 255
      "0xFFFFFFFFFFFFFFFF" `scansAs` TokenIntVal Hex (-1)


tokenClass :: String -> Either String TokenClass
tokenClass s = case lexWrap s of
  Right (Token _ cls) -> Right cls
  Left msg -> Left msg


lexWrap :: String -> Either String Token
lexWrap = runAlex' alexMonadScan' ""
