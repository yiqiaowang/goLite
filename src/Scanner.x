{
module Scanner
  ( Token(..)
  , TokenClass(..)
  , IntType(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where
import Prelude hiding ( lex )
import Control.Monad ( liftM )
import Numeric (readOct, readHex)
}

%wrapper "monadUserState"

$digit = [0-9] -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$white = [\ \t \r]
@newline = \n

$string_val = [^"\\\/\n]
$string_escape = [\" a b f n r t v \\]

-- runes can be anything but ' or \
$rune_val = [^'\\\n]
$rune_escape = [\' a b f n r t v \\] 

-- raw strings
$raw_val = [^`]


-- Comment  \\\\[^\n]*
@comment_start = \/{2}
$comment_content = [~\n]
@comment = @comment_start$comment_content*

$comment_tail = [^\/\*]

-- Block Comment
@b_comment = \/\*([^\*]|\n|\*[^\/])*\*\/

-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  $white+                         ;
  @comment 			  ;
  @b_comment			  ;

-- goLang keywords, reserved
  break				  { lex' TokenBreak }	
  case				  { lex' TokenCase }
  chan				  { lex' TokenChan }
  const				  { lex' TokenConst }
  continue			  { lex' TokenContinue }
  default 			  { lex' TokenDefault }
  defer				  { lex' TokenDefer }
  else                            { lex' TokenElse }
  fallthrough			  { lex' TokenFallthrough }
  for				  { lex' TokenFor }
  func				  { lex' TokenFunc }
  go				  { lex' TokenGo }
  goto				  { lex' TokenGoto }
  if                              { lex' TokenIf }
  import			  { lex' TokenImport }
  interface			  { lex' TokenInterface }
  map				  { lex' TokenMap }
  package			  { lex' TokenPackage }
  range				  { lex' TokenRange }
  return			  { lex' TokenReturn }
  select			  { lex' TokenSelect }
  struct			  { lex' TokenStruct }
  switch			  { lex' TokenSwitch }
  type				  { lex' TokenType }
  var                             { lex' TokenVar }

-- goLite keywords, reserved
  print                           { lex' TokenPrint }
  println			  { lex' TokenPrintln }
  append			  { lex' TokenAppend }

-- operators
  \+                              { lex' TokenAdd }
  \-                              { lex' TokenSub }
  \*                              { lex' TokenMult }
  \/                              { lex' TokenDiv }
  \%				  { lex' TokenMod }

  &				  { lex' TokenBitAnd } 
  \|				  { lex' TokenBitOr }
  \^				  { lex' TokenBitXor }
  \<\<				  { lex' TokenBitLShift }
  >>				  { lex' TokenBitRShift }
  &\^				  { lex' TokenBitClear }
  
  \+=				  { lex' TokenAddEq }
  \-=				  { lex' TokenSubEq }
  \*=				  { lex' TokenMultEq }
  \/=				  { lex' TokenDivEq }
  \%=				  { lex' TokenModEq }
  
  &=				  { lex' TokenBitAndEq }
  \|=				  { lex' TokenBitOrEq }
  \^=				  { lex' TokenBitXorEq }
  \<\<=				  { lex' TokenBitLShiftEq }
  >>=				  { lex' TokenBitRShiftEq }
  &\^=				  { lex' TokenBitClearEq }

  &&				  { lex' TokenLogAnd }
  \|\|				  { lex' TokenLogOr }
  \<\-				  { lex' TokenChannel }
  \+\+				  { lex' TokenInc }
  \-\-				  { lex' TokenDec }
  
  ==				  { lex' TokenBoolEq }
  \<				  { lex' TokenBoolLT }
  >				  { lex' TokenBoolGT }
  =                               { lex' TokenEq }
  !				  { lex' TokenBoolNot }

  !=				  { lex' TokenBoolNotEq }
  \<=				  { lex' TokenBoolLTE }
  >=				  { lex' TokenBoolGTE }
  :=				  { lex' TokenShortDec }
  \.\.\.			  { lex' TokenVariadic }

  \(                              { lex' TokenLParen }
  \)                              { lex' TokenRParen }
  \[				  { lex' TokenLSquare }
  \]				  { lex' TokenRSquare }
  \{				  { lex' TokenLCurly }
  \}				  { lex' TokenRCurly }
  \.				  { lex' TokenPeriod }
  \,				  { lex' TokenComma }
  \:                              { lex' TokenColon }
  \;                              { lex' TokenSemicolon }

-- Integers
-- Decimal
 0|[1-9][0-9]*                    { lex (genIntLex Decimal) }

-- Octal
 0[0-7]+	       	          { lex (genIntLex Octal) }

-- Hex
 0[xX][0-9a-fA-F]+		 { lex ((genIntLex Hex) . extractHexNumeric) }

-- Float 
((0|([1-9][0-9]*))\.[0-9]+)      { lex (TokenFloatVal . read) }
(\.[0-9]+)			 { lex (TokenFloatVal . read . prependZero) }
([1-9][0-9]*\.)			 { lex (TokenFloatVal . read . appendZero) }

-- String Values
  \"($string_val|
	\\$string_escape|
	\/$comment_tail)*\"       { lex TokenStringVal }
-- Runes
  \'($rune_val|\\$rune_escape)\'  { lex (TokenRuneVal . read) }

-- Raw strings
  \`$raw_val*\`			  { lex TokenRawVal}

-- Identifiers
  [$alpha \_]
    [$alpha $digit \_ \’]*        { lex TokenId }




{
-- Append 0 to the end of a string
appendZero :: String -> String
appendZero s = s ++ "0"

-- Append 0 to the front of a string
prependZero :: String -> String
prependZero s = "0" ++ s

-- Extract the integer value result of readOct
extractOct2Int :: [(Integer, String)] -> Integer
extractOct2Int ((i,s):xs) = i 

-- Extract the integer value result of readHex
extractHex2Int :: [(Integer, String)] -> Integer
extractHex2Int ((i,s):xs) = i 

-- Extract the numeric value from a hex number
extractHexNumeric :: String -> String
extractHexNumeric s = drop 2 s

-- Return a function that can be consumed by lex
genIntLex :: IntType -> (String -> TokenClass)
genIntLex Decimal = (TokenIntVal Decimal . read)
genIntLex Octal = (TokenIntVal Octal . extractOct2Int . readOct)
genIntLex Hex = (TokenIntVal Hex . extractHex2Int . readHex)

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState
  = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"


getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState


setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState


-- Token includes source code position and a token class
data Token = Token AlexPosn TokenClass
  deriving (Eq, Show)


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

-- Required by Alex spec
alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF


-- Unfortunately, we have to extract the matching bit of string ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex cons = \(p, _, _, s) i -> return $ Token p (cons (take i s))


-- For constructing tokens that do not depend on the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
      alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len


-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)


-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
