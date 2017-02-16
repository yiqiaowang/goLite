{
module Compiler.Lexer
  ( Token(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where


import Prelude hiding (lex)
import Control.Monad ( liftM )

}

%wrapper "monadUserState"

$digit = [0-9] -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$white = [\ \t\r]
@newline = \n
$string_val = [^"\\\/]
$escaped = [\" a b f n r t v \\]
$comment_tail = [^\/\*]
@b_comment_val = \/\*(.|$newline)*?\*\/

-- Comment  \\\\[^\n]*
@comment_start = @forward_slash{2}
$comment_content = [~\n]
@comment = @comment_start$comment_content*

-- All token actions have type ( AlexPosn -> String -> Token )
tokens :-
  $white+                         ;
  @comment 			  ;
  @b_comment_val		  ;

-- goLang keywords, reserved
  break				  { lex' TokenBreak }	
  case				  { lex' TokenCase }
  chan				  { lex' TokenChan }
  const				  { lex' TokenConst }
  continue			  { lex' TokenContinue }
  default 			  { lex' TokenDefault }
  defer				  { lex' TokenDefer }
  else                            { lex' TokenElse }
  fallthrough			  { lex' TokenFallThrough }
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
  %				  { lex' TokenMod }

  &				  { lex' TokenBitAnd } 
  \|				  { lex' TokenBitOr }
  \^				  { lex' TokenBitXOr }
  <<				  { lex' TokenBitLShift }
  >>				  { lex' TokenBitRShift }
  &\^				  { lex' TokenBitClear }
  
  \+=				  { lex' TokenAddEq }
  -=				  { lex' TokenSubEq }
  \*=				  { lex' TokenMultEq }
  \/=				  { lex' TokenDivEq }
  \%=				  { lex' TokenModEq }
  
  &=				  { lex' TokenBitAndEq }
  \|=				  { lex' TokenBitOrEq }
  \^=				  { lex' TokenBitXOrEq }
  <<=				  { lex' TokenBitLShiftEq }
  >>=				  { lex' TokenBitRShiftEq }
  &\^=				  { lex' TokenBitClearEq }

  &&				  { lex' TokenLogAnd }
  \|\|				  { lex' TokenLogOr }
  <-				  { lex' TokenChannel }
  \+\+				  { lex' TokenInc }
  --				  { lex' TokenDec }
  
  ==				  { lex' TokenBoolEq }
  <				  { lex' TokenBoolLThan }
  >				  { lex' TokenBoolGThan }
  =                               { lex' TokenEq }
  !				  { lex' TokenBoolNot }

  !=				  { lex' TokenBoolNotEq }
  <=				  { lex' TokenBoolLTE }
  >=				  { lex' Token }
  :=				  { lex' Token }
  \.\.\.			  { lex' Token }
  
  \(                              { lex' TokenLParen }
  \)                              { lex' TokenRParen }
  \[				  { lex' Token }
  \]				  { lex' Token }
  \.				  { lex' Token }
  ,				  { lex' Token }
  \:                              { lex' TokenColon }
  \;                              { lex' TokenSemicolon }


  -- Comments
  \/\/				  { lex' Token }
  \/\*				  { lex' Token }
  \*\/				  { lex' Token }
  
  0|[1-9][0-9]*                   { lex (TokenIntVal . read) }

  (0|([1-9][0-9]*))\.[0-9]+       { lex (TokenFloatVal . read) }

  \"($string_val|
	\\$escaped|
	\/$comment_tail)*\"       { lex TokenStringVal }

  [$alpha \_]
    [$alpha $digit \_ \’]*        { lex TokenId }

{

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }


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
  = TokenVar
  | TokenId String
  | TokenFloatType
  | TokenFloatVal Float
  | TokenIntType
  | TokenIntVal Int
  | TokenStringType
  | TokenStringVal String
  | TokenIf
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenSemicolon
  | TokenColon
  | TokenRead
  | TokenPrint
  | TokenEOF
  deriving (Eq,Show)


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
