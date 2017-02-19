{
{-# OPTIONS -w #-}

module Parser
  ( parse
  ) where

import Language
import Scanner
}

%name calc
%tokentype { Token }

%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { parseError }


%token
        -- TODO added by Charlie for testing
        -- Need at least one symbol for Happy to compile parser
        break             { Token _ TokenVar }

%%

Program
      : {}

{

-- Wrapper function
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)


--
parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parser error at token '" ++ show t ++ "'")


--
parse :: FilePath -> String -> Either String Program
parse fp text = undefined

}
