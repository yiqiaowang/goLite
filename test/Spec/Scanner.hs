module Spec.Scanner where


import Data.Char(digitToInt)
import Control.Monad(forM_)
import Test.Hspec
import Scanner


spec :: Spec
spec =
  describe "Scanner" $ do
    forM_ tests $ \(text, cls) ->
      it ("correctly scans \"" ++ text ++ "\" as token class " ++ show cls ++ ":") $
        tokenClass text `shouldBe` Right cls


runes :: [Char]
runes =
  ['0'..'9'] ++
  ['a'..'z'] ++
  ['A'..'Z'] ++
  ['\a', '\b', '\f', '\n', '\r', '\t', '\v', '\\', '\'']


tests :: [(String, TokenClass)]
tests =
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
  , ("0", TokenIntVal Decimal 0)
  , ("100", TokenIntVal Decimal 100)

  , ("01", TokenIntVal Octal 1)
  , ("0377", TokenIntVal Octal 255)
  , ("032457", TokenIntVal Octal 13615)

  , ("0x0", TokenIntVal Hex 0)
  , ("0x1", TokenIntVal Hex 1)
  , ("0xFF", TokenIntVal Hex 255)
  , ("0xFFFFFFFFFFFFFFFF", TokenIntVal Hex (-1))

  --Floats
  , ("0.12", TokenFloatVal 0.12)
  , (".12", TokenFloatVal 0.12)
  , ("12.", TokenFloatVal 12.0)

  --Strings
  , ("\"hello\"", TokenStringVal "\"hello\"")
  , (show runes, TokenStringVal $ show runes)

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
