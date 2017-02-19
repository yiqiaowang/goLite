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
        package                     { Token _ TokenPackage }
        ';'                         { Token _ TokenSemicolon }
        var                         { Token _ TokenVar}
        '('                         { Token _ TokenLParen}
        ')'                         { Token _ TokenRParen}
        '='                         { Token _ TokenEq }

        id                          { Token _ (TokenId $$) }
        int                         { Token _ (TokenInt Decimal $$) }
        oct                         { Token _ (TokenInt Octal $$) }
        hex                         { Token _ (TokenInt Hex $$) }
        float                       { Token _ (TokenFloat $$) }
        string                      { Token _ (TokenString $$) }
        rune                        { Token _ (TokenRune $$) }
        raw                         { Token _ (TokenRaw $$) }


%%

Program
      : Package Alls                { Program $1 $2 }


Package
      : package id ';'              { $2 }


Alls  : Alls All                    { $2 : $1 }
      | {- Empty -}                 { [] }


All   : Stmt                        { Stmt $1 }


Stmt  : VarDec                      { $1 }


VarDec
      : var id Type ';'             { VarDec [] Nothing [] }
      | var id '=' Exp ';'          { VarDec [] Nothing [] }
      | var id Type '=' Exp ';'     { VarDec [] Nothing [] }


Exp   : Lit                         { Literal $1 }


Lit   : int                         { Int $1 }
      | oct                         { Int $1 }
      | hex                         { Int $1 }
      | float                       { Float64 $1 }
      | rune                        { Rune $1 }
      | string                      { String $1 }
      | raw                         { Raw $1 }


Type  : id                          { $1 }


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
parse = runAlex' calc

}
