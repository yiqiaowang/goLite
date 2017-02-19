module Spec.Scanner where


import Data.Char(digitToInt)
import Control.Monad(forM_)
import Test.Hspec
import Scanner


spec :: Spec
spec =
  describe "Scanner" $ do
    forM_ tokenTests $ \(text, cls) ->
      it ("correctly scans \"" ++ text ++ "\" as token class " ++ show cls ++ ":") $
        tokenClass text `shouldBe` Right cls
    forM_ whiteTests $ \white ->
      it ("correctly scans '" ++ show white ++ "' by ignoring it") $
        tokenClass white `shouldBe` Right TokenEOF


whiteTests :: [String]
whiteTests =
  [ " "
  , "\n"
  , "\r"
  , "\t"
  ]


runes :: [Char]
runes =
  ['0'..'9'] ++
  ['a'..'z'] ++
  ['A'..'Z'] ++
  ['\a', '\b', '\f', '\n', '\r', '\t', '\v', '\\', '\'']


tokenTests :: [(String, TokenClass)]
tokenTests =
    -- Go keywords
  [ ("break", TokenBreak)
  , ("case", TokenCase)
  , ("chan", TokenChan)
  , ("const", TokenConst)
  , ("continue", TokenContinue)
  , ("default", TokenDefault)
  , ("defer", TokenDefer)
  , ("else", TokenElse)
  , ("fallthrough", TokenFallthrough)
  , ("for", TokenFor)
  , ("func", TokenFunc)
  , ("go", TokenGo)
  , ("goto", TokenGoto)
  , ("if", TokenIf)
  , ("import", TokenImport)
  , ("interface", TokenInterface)
  , ("map", TokenMap)
  , ("package", TokenPackage)
  , ("range", TokenRange)
  , ("return", TokenReturn)
  , ("select", TokenSelect)
  , ("struct", TokenStruct)
  , ("switch", TokenSwitch)
  , ("type", TokenType)
  , ("var", TokenVar)

  -- goLite keywords
  , ("print", TokenPrint)
  , ("println", TokenPrintln)
  , ("append", TokenAppend)

  -- operators
  , ("+", TokenAdd)
  , ("-", TokenSub)
  , ("*", TokenMult)
  , ("/", TokenDiv)
  , ("%", TokenMod)

  , ("&", TokenBitAnd)
  , ("|", TokenBitOr)
  , ("^", TokenBitXor)
  , ("<<", TokenBitLShift)
  , (">>", TokenBitRShift)
  , ("&^", TokenBitClear)

  , ("+=", TokenAddEq)
  , ("-=", TokenSubEq)
  , ("*=", TokenMultEq)
  , ("/=", TokenDivEq)
  , ("%=", TokenModEq)

  , ("&=", TokenBitAndEq)
  , ("|=", TokenBitOrEq)
  , ("^=", TokenBitXorEq)
  , ("<<=", TokenBitLShiftEq)
  , (">>=", TokenBitRShiftEq)
  , ("&^=", TokenBitClearEq)

  , ("&&", TokenLogAnd)
  , ("||", TokenLogOr)
  , ("<-", TokenChannel)
  , ("++", TokenInc)
  , ("--", TokenDec)

  , ("==", TokenBoolEq)
  , ("<", TokenBoolLT)
  , (">", TokenBoolGT)
  , ("=", TokenEq)
  , ("!", TokenBoolNot)

  , ("!=", TokenBoolNotEq)
  , ("<=", TokenBoolLTE)
  , (">=", TokenBoolGTE)
  , (":=", TokenShortDec)
  , ("...", TokenVariadic)

  , ("(", TokenLParen)
  , (")", TokenRParen)
  , ("[", TokenLSquare)
  , ("]", TokenRSquare)
  , ("{", TokenLCurly)
  , ("}", TokenRCurly)
  , (".", TokenPeriod)
  , (",", TokenComma)
  , (";", TokenSemicolon)
  , (":", TokenColon)

  -- Ints
  , ("0", TokenInt Decimal 0)
  , ("100", TokenInt Decimal 100)

  , ("01", TokenInt Octal 1)
  , ("0377", TokenInt Octal 255)
  , ("032457", TokenInt Octal 13615)

  , ("0x0", TokenInt Hex 0)
  , ("0x1", TokenInt Hex 1)
  , ("0xFF", TokenInt Hex 255)
  , ("0xFFFFFFFFFFFFFFFF", TokenInt Hex (-1))

  --Floats
  , ("0.12", TokenFloat 0.12)
  , (".12", TokenFloat 0.12)
  , ("12.", TokenFloat 12.0)

  --Strings
  , ("\"hello\"", TokenString "\"hello\"")
  , (show runes, TokenString $ show runes)

  --Identifiers
  , ("_", TokenId "_")
  , ("__", TokenId "__")
  , ("a", TokenId "a")
  , ("a_z", TokenId "a_z")
  , ("aZ", TokenId "aZ")
  , ("a0", TokenId "a0")
  , ("_123456789", TokenId "_123456789")
  , ("theQuickFoxJumpsOverTheLazyBrownDog", TokenId "theQuickFoxJumpsOverTheLazyBrownDog")
  ]


tokenClass :: String -> Either String TokenClass
tokenClass s = case lexWrap s of
  -- Don't care about character positions for testing
  Right (Token _ cls) -> Right cls
  Left msg -> Left msg


lexWrap :: String -> Either String Token
lexWrap = runAlex' alexMonadScan' ""
