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
        func                        { Token _ TokenFunc}
        return                      { Token _ TokenReturn}
        print                       { Token _ TokenPrint }
        println                     { Token _ TokenPrintln }

        '('                         { Token _ TokenLParen}
        ')'                         { Token _ TokenRParen}
        '='                         { Token _ TokenEq }
        ','                         { Token _ TokenComma}
        '{'                         { Token _ TokenLCurly}
        '}'                         { Token _ TokenRCurly}
        '['                         { Token _ TokenLSquare}
        ']'                         { Token _ TokenRSquare}
        '++'                        { Token _ TokenInc}
        '--'                        { Token _ TokenDec}
        '+='                        { Token _ TokenAddEq}

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


All   : Stmt                                            { Stmt $1 }
      | func id '(' ParamList ')' '{' Stmts '}'         { Function $2 $4 Nothing $7 }
      | func id '(' ParamList ')' Type '{' Stmts '}'    { Function $2 $4 (Just $6) $8 }
      | func id '(' ')' '{' Stmts '}'                   { Function $2 [] Nothing $6 }
      | func id '(' ')' Type '{' Stmts '}'              { Function $2 [] (Just $5) $7 }


ParamList
      : Param ',' ParamList               { $1 : $3 }
      | Param                             { [$1] }


Param : VarList Type                      { Parameter $1 $2 }


Stmts : Stmt Stmts                        { $1 : $2 }
      | {- Empty -}                       { [] }


Stmt  : var VarDec ';'                    { VarDec $2 }
      | var '(' VarDecList ')'            { VarDecList $3 }
      | type TypeDec ';'                  { TypeDec $2 }
      | type '(' TypeDecList ')'          { TypeDecList $3 }
      | var id '[' Exp ']' Type ';'       { Array $2 $4 $6 }
      | var id '[' ']' Type ';'           { Slice $2 $5 }
      | return ';'                        { Return Nothing}
      | return Exp ';'                    { Return (Just $2)}
      | SimpleStmt ';'                    { SimpleStmt $1 }
      | print '(' ExpListEmpty ')' ';'    { Print $3 }
      | println '(' ExpListEmpty ')' ';'  { Println $3 }


SimpleStmt
      : Exp                       { ExpStmt $1 }
      | id '++'                   { Incr $1 }
      | id '--'                   { Decr $1 }
      | VarList '=' ExpList       { Assign $1 $3 }
      | id '+=' Exp               { PlusEq $1 $3 }


VarDec
      : VarList Type                  { Variable $1 (Just $2) [] }
      | VarList '=' ExpList           { Variable $1 Nothing $3 }
      | VarList Type '=' ExpList      { Variable $1 (Just $2) $4 }


VarDecList
      : VarDec VarDecList                 { $1 : $2 }
      | {- Empty -}                       { [] }


ExpList
      :: { [Expression] }
      : Exp ',' ExpList               { $1 : $3 }
      | Exp                           { [$1] }


ExpListEmpty
      :: { [Expression] }
      : Exp ',' ExpList                 { $1 : $3 }
      | Exp                             { [$1] }
      | {- Empty -}                     { [] }


VarList
      : id ',' VarList              { $1 : $3 }
      | id                          { [$1] }


TypeDec
      : id Type ';'                    { TypeName $1 $2 }
      | id struct '{' StructList '}'   { Struct $1 $4 }


TypeDecList
      : TypeDec TypeDecList         { $1 : $2 }
      | {- Empty -}                       { [] }


StructList
      : VarList Type ';' StructList { ($1, $2) : $4 }
      | {- Empty -}                 { [] }


Exp   : Lit                         { Literal $1 }
      | id                          { Id $1 }


Num   : int                         { Int $1 }
      | oct                         { Int $1 }
      | hex                         { Int $1 }


Lit   : Num                         { $1 }
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
