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
        type                        { Token _ TokenType}
        struct                      { Token _ TokenStruct}
        '('                         { Token _ TokenLParen}
        ')'                         { Token _ TokenRParen}
        '='                         { Token _ TokenEq }
        ','                         { Token _ TokenComma}
        '{'                         { Token _ TokenLCurly}
        '}'                         { Token _ TokenRCurly}

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


Stmt  : var VarDec ';'                    { VarDec $2 }
      | var '(' VarDecList ')'            { VarDecList $3 }
      | type TypeDec                      { TypeDec $2 }
      | type '(' TypeDecList ')'          { TypeDecList $3 }
 
VarDec
      : VarList Type                  { Variable $1 (Just $2) [] }
      | VarList '=' ExpList           { Variable $1 Nothing $3 }
      | VarList Type '=' ExpList      { Variable $1 (Just $2) $4 }

VarDecList
      : VarDec VarDecList                 { $1 : $2 }
      | VarDec                            { [$1] }

ExpList
      : Exp ',' ExpList               { $1 : $3 }
      | Exp                           { [$1] }

VarList
      : id ',' VarList              { $1 : $3 }
      | id                          { [$1] }

TypeDec
      : id Type ';'                    { TypeName $1 $2 }
      | id struct '{' StructList '}'   { Struct $1 $4 }

TypeDecList
      : TypeDec TypeDecList         { $1 : $2 }
      | TypeDec                     { [$1] }

StructList
      : VarList Type ';' StructList    { ($1, $2) : $4 }
      | VarList Type ';'               { [($1, $2)] }

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
