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
        var                         { Token _ TokenVar }
        type                        { Token _ TokenType }
        struct                      { Token _ TokenStruct }
        func                        { Token _ TokenFunc }
        return                      { Token _ TokenReturn }
        print                       { Token _ TokenPrint }
        println                     { Token _ TokenPrintln }
        if                          { Token _ TokenIf }
        else                        { Token _ TokenElse }
        switch                      { Token _ TokenSwitch }
        case                        { Token _ TokenCase }
        default                     { Token _ TokenDefault }
        for                         { Token _ TokenFor }
        break                       { Token _ TokenBreak }
        continue                    { Token _ TokenContinue }
	append			    { Token _ TokenAppend }

        '('                         { Token _ TokenLParen }
        ')'                         { Token _ TokenRParen }
        '='                         { Token _ TokenEq }
        ','                         { Token _ TokenComma }
        '{'                         { Token _ TokenLCurly }
        '}'                         { Token _ TokenRCurly }
        '['                         { Token _ TokenLSquare }
        ']'                         { Token _ TokenRSquare } 
        '++'                        { Token _ TokenInc }
        '--'                        { Token _ TokenDec }
        '+='                        { Token _ TokenAddEq }
        '-='                        { Token _ TokenSubEq }
        '*='                        { Token _ TokenMultEq }
        '/='                        { Token _ TokenDivEq }
        '%='                        { Token _ TokenModEq }
        '&='                        { Token _ TokenBitAndEq }
        '|='                        { Token _ TokenBitOrEq }
        '^='                        { Token _ TokenBitXorEq }
        '<<='                       { Token _ TokenBitLShiftEq }
        '>>='                       { Token _ TokenBitRShiftEq }
        '&^='                       { Token _ TokenBitClearEq }
        ':='                        { Token _ TokenShortDec }
        ':'                         { Token _ TokenColon }

-- Binary Operations
	'||'			    { Token _ TokenLogOr }
	'&&'			    { Token _ TokenLogAnd }
	'=='			    { Token _ TokenBoolEq }
	'!='			    { Token _ TokenBoolNotEq }
	'<'			    { Token _ TokenBoolLT }
	'<='			    { Token _ TokenBoolLTE }
	'>'			    { Token _ TokenBoolGT }
	'>='			    { Token _ TokenBoolGTE }
	'+'			    { Token _ TokenAdd }
	'-'			    { Token _ TokenSub }
	'|'			    { Token _ TokenBitOr }
	'^'			    { Token _ TokenBitXor }
	'*'			    { Token _ TokenMult }
	'/'			    { Token _ TokenDiv }
	'%'			    { Token _ TokenMod }
	'<<'			    { Token _ TokenBitLShift }
	'>>'			    { Token _ TokenBitRShift }
	'&'			    { Token _ TokenBitAnd }
	'&^'			    { Token _ TokenBitClear }
-- Unary Operations
	'!'			    { Token _ TokenBoolNot }
	'<-'			    { Token _ TokenChannel }

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
      | func id '(' ParamList ')' '{' Stmts '}'         { Function $2 $4 Nothing $7 }
      | func id '(' ParamList ')' Type '{' Stmts '}'    { Function $2 $4 (Just $6) $8 }
      | func id '(' ')' '{' Stmts '}'                   { Function $2 [] Nothing $6 }
      | func id '(' ')' Type '{' Stmts '}'              { Function $2 [] (Just $5) $7 }

ParamList : Param ',' ParamList               { $1 : $3 }
      | Param                             { [$1] }


Param : VarList Type                      { Parameter $1 $2 }


Stmts : Stmt Stmts                        { $1 : $2 }
      | {- Empty -}                       { [] }


Stmt  : var VarDec ';'                            { VarDec $2 }
      | var '(' VarDecList ')'                    { VarDecList $3 }
      | type TypeDec ';'                          { TypeDec $2 }
      | type '(' TypeDecList ')'                  { TypeDecList $3 }
      | var id '[' Expr ']' Type ';'               { Array $2 $4 $6 }
      | var id '[' ']' Type ';'                   { Slice $2 $5 }
      | return ';'                                { Return Nothing }
      | return Expr ';'                            { Return (Just $2) }
      | SimpleStmt ';'                            { SimpleStmt $1 }
      | print '(' ExprListEmpty ')' ';'            { Print $3 }
      | println '(' ExprListEmpty ')' ';'          { Println $3 }
      | If                                        { If $1 }
      | switch '{' ClauseList '}'                 { Switch Nothing Nothing $3 }
      | switch SimpleStmt ';' '{' ClauseList '}'  { Switch (Just $2) Nothing $5 } 
      | switch Expr '{' ClauseList '}'             { Switch Nothing (Just $2) $4 }
      | switch SimpleStmt ';' Expr'{' ClauseList '}'   { Switch (Just $2) (Just $4) $6 }
      | for '{' Stmts '}'                         { Infinite $3 }
      | for Expr '{' Stmts '}'                     { While $2 $4 }
      | for SimpleStmt ';' Expr ';' SimpleStmt '{' Stmts '}'         { For $2 $4 $6 $8 }
      | break ';'                                 { Break }
      | continue ';'                              { Continue }

ClauseList
      : Clause ';' ClauseList             { $1 : $3 }
      | {- Empty -}                       { [] }

Clause
      : case ExprList ':' Stmts          { Case $2 $4 }
      | default ':' Stmts               { Default $3 }

If    : if SimpleStmt ';' Expr '{' Stmts '}'                            { IfStmt (Just $2) $4 $6 Nothing }
      | if SimpleStmt ';' Expr '{' Stmts '}' else '{' Stmts '}'       { IfStmt (Just $2) $4 $6 (Just (Right $10)) }
      | if SimpleStmt ';' Expr '{' Stmts '}' else If                  { IfStmt (Just $2) $4 $6 (Just (Left $9)) }
      | if Expr '{' Stmts '}'                                       { IfStmt Nothing $2 $4 Nothing }
      | if Expr '{' Stmts '}' else '{' Stmts '}'                  { IfStmt Nothing $2 $4 (Just (Right $8)) }
      | if Expr '{' Stmts '}' else If                             { IfStmt Nothing $2 $4 (Just (Left $7)) }

SimpleStmt
      : Expr                       { ExprStmt $1 }
      | id '++'                   { Incr $1 }
      | id '--'                   { Decr $1 }
      | VarList '=' ExprList       { Assign $1 $3 }
      | id '+=' Expr               { PlusEq $1 $3 }
      | id '-=' Expr               { MinusEq $1 $3 }
      | id '*=' Expr               { MulEq $1 $3 }
      | id '/=' Expr               { DivEq $1 $3 }
      | id '%=' Expr               { ModEq $1 $3 }
      | id '&=' Expr               { BitAndEq $1 $3 }
      | id '|=' Expr               { BitOrEq $1 $3 }
      | id '^=' Expr               { BitXOrEq $1 $3 }
      | id '<<=' Expr              { BitLShiftEq $1 $3 }
      | id '>>=' Expr              { BitRShiftEq $1 $3 }
      | id '&^=' Expr              { BitClearEq $1 $3 }
      | VarList ':=' ExprList      { ShortVarDec $1 $3 }


VarDec
      : VarList Type                  { Variable $1 (Just $2) [] }
      | VarList '=' ExprList           { Variable $1 Nothing $3 }
      | VarList Type '=' ExprList      { Variable $1 (Just $2) $4 }


VarDecList
      : VarDec VarDecList                 { $1 : $2 }
      | {- Empty -}                       { [] }


ExprList
      :: { [Expression] }
      : Expr ',' ExprList               { $1 : $3 }
      | Expr                           { [$1] }

ExprListEmpty
      :: { [Expression] }
      : Expr ',' ExprList                 { $1 : $3 }
      | Expr                             { [$1] }
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


Expr
       :: { Expression }
       : UnaryExpr      { $1 }
       | Expr BinaryOp Expr { $2 $1 $3 }

UnaryExpr : PrimaryExpr { $1 }
	  | UnaryOp UnaryExpr { $1 $2 }

PrimaryExpr : '(' Expr ')' { $2 }
	    | id	   { Id $1 }
	    | Lit	   { Literal $1 }
	    | FuncCall	   { $1 }
	    | Append	   { $1 }

BinaryOp : '||'         { Or }
	 | '&&'		{ And }
	 | RelOp	{ $1 }
	 | AddOp	{ $1 }
	 | MulOp	{ $1 }

RelOp	 : '=='		{ Equals }
	 | '!='		{ NotEquals }
	 | '<'		{ LThan }
	 | '<='		{ LEThan }
	 | '>'		{ GThan }
	 | '>='		{ GEThan }

AddOp	 : '+'		{ Add }
	 | '-'		{ Sub }
	 | '|'		{ BitOr }
	 | '^'		{ BitXor }

MulOp	 : '*'		{ Mult }
	 | '/'		{ Div }
	 | '%'		{ Mod }
	 | '<<'		{ BitLShift }
	 | '>>'		{ BitRShift }
	 | '&'		{ BitAnd }
	 | '&^'		{ BitClear }


UnaryOp	 : '+'		{ UnaryPos }
	 | '-'		{ UnaryNeg }
	 | '!'		{ BoolNot }
	 | '^'		{ BitComplement }
	 | '*'		{ Pointer }
	 | '&'		{ Address }
	 | '<-'		{ Channel }


Num   : int                         { Int $1 }
      | oct                         { Int $1 }
      | hex                         { Int $1 }


Lit   : Num                         { $1 }
      | float                       { Float64 $1 }
      | rune                        { Rune $1 }
      | string                      { String $1 }
      | raw                         { Raw $1 }


Type  : id                          { $1 }

Append : append '(' Expr ',' Expr ')' { Append $3 $5 }

-- Issue: FuncCall doesn't work if we need to support things like
-- math.pow(x,y). This is due to the constructor taking arguments
-- Identifier Expression, instead of Expression Expression.

FuncCall : id '(' ExprListEmpty ')' ';' { FuncCall $1 $3}

-- Block : '{' Stmts '}'

-- If    : if SimpleStmt ';'


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
