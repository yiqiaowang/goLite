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
import Data.Char (ord)
import Numeric (readOct, readHex)
import Data.Char (ord)
import TokenClass
}

%wrapper "monadUserState"

$digit = [0-9] -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$white = [\ \t \r]
@newline = \n
@newOrWhite = ($white|@newline)

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

-- Block Comment
@b_comment = \/\*(@newOrWhite|[~\*]|\*([~\/]|@newOrWhite))*\*\/

-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  @b_comment			  ;
  $white+           ;
  @comment 			    ;
  @newline          ;

-- goLang keywords, reserved
  break				  { lex' TokenBreak }
  case				  { lex' TokenCase }
  chan				  { lex' TokenChan }
  const				  { lex' TokenConst }
  continue			  { lex' TokenContinue }
  default			  { lex' TokenDefault }
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
 0[0-7]+			  { lex (genIntLex Octal) }

-- Hex
 0[xX][0-9a-fA-F]+		 { lex ((genIntLex Hex) . extractHexNumeric) }

-- Float
((0|([1-9][0-9]*))\.[0-9]+)      { lex (TokenFloat . read) }
(\.[0-9]+)			 { lex (TokenFloat . read . prependZero) }
([1-9][0-9]*\.)			 { lex (TokenFloat . read . appendZero) }

-- String Values
  \"($string_val|
	\\$string_escape)*\"       { lex TokenString }
-- Runes
  \'($rune_val|\\$rune_escape)\'  { lex (TokenRune . ord . handleRune) }

-- Raw strings
  \`$raw_val*\`			  { lex TokenRaw}

-- Identifiers
  [$alpha \_]
    [$alpha $digit \_ \’]*        { lex TokenId }




{
-- Handle runes
handleRune :: [Char] -> Char
handleRune (x:y:z) = y

-- Append 0 to the end of a string
appendZero :: String -> String
appendZero s = s ++ "0"

-- Append 0 to the front of a string
prependZero :: String -> String
prependZero s = "0" ++ s

-- Extract the integer value result of readOct
extractOct2Int :: [(Int, String)] -> Int
extractOct2Int ((i,s):xs) = i

-- Extract the integer value result of readHex
extractHex2Int :: [(Int, String)] -> Int
extractHex2Int ((i,s):xs) = i

-- Extract the numeric value from a hex number
extractHexNumeric :: String -> String
extractHexNumeric s = drop 2 s

-- Return a function that can be consumed by lex
genIntLex :: IntType -> (String -> TokenClass)
genIntLex Decimal = (TokenInt Decimal . read)
genIntLex Octal = (TokenInt Octal . extractOct2Int . readOct)
genIntLex Hex = (TokenInt Hex . extractHex2Int . readHex)

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState
  = AlexUserState
  { previousToken :: Maybe Token }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing


getPreviousToken :: Alex (Maybe Token)
getPreviousToken = liftM previousToken alexGetUserState


setPreviousToken :: Maybe Token -> Alex ()
setPreviousToken = alexSetUserState . AlexUserState


-- Token includes source code position and a token class
data Token = Token AlexPosn TokenClass
  deriving (Eq, Show)


-- Required by Alex spec
alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

alexSemicolon :: Alex Token
alexSemicolon = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenSemicolon


-- Unfortunately, we have to extract the matching bit of string ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex cons = \(p, _, _, s) i -> return $ Token p (cons (take i s))


-- For constructing tokens that do not depend on the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const


--
isOptionalSemicolonToken :: TokenClass -> Bool
isOptionalSemicolonToken (TokenId _) = True
isOptionalSemicolonToken (TokenFloat _) = True
isOptionalSemicolonToken (TokenInt _ _) = True
isOptionalSemicolonToken (TokenString _) = True
isOptionalSemicolonToken (TokenRune _) = True
isOptionalSemicolonToken (TokenRaw _) = True
isOptionalSemicolonToken (TokenContinue) = True
isOptionalSemicolonToken (TokenBreak) = True
isOptionalSemicolonToken (TokenFallthrough) = True
isOptionalSemicolonToken (TokenReturn) = True
isOptionalSemicolonToken (TokenInc) = True
isOptionalSemicolonToken (TokenDec) = True
isOptionalSemicolonToken (TokenRParen) = True
isOptionalSemicolonToken (TokenRSquare) = True
isOptionalSemicolonToken (TokenRCurly) = True
isOptionalSemicolonToken _ = False


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

    -- will be called at each new line
    AlexSkip  inp' len -> do
      alexSetInput inp'
      prevToken <- getPreviousToken
      if (ord $ alexInputPrevChar inp') `elem` [0,3,4,10]
        then case prevToken of
          Nothing -> alexMonadScan'
          Just (Token p cls) ->
            if isOptionalSemicolonToken cls
              then do
                setPreviousToken $ Just $ Token p TokenSemicolon
                alexSemicolon
              else alexMonadScan'
        else
          alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      token <- action (ignorePendingBytes inp) len
      setPreviousToken $ Just token
      return token


-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  alexError (show l ++ ":" ++ show c ++ ": " ++ msg)


-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setPreviousToken Nothing >> a)
}
