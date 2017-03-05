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
      '.'                         { Token _ TokenPeriod }

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

	id_raw                      { Token _ (TokenId $$) }
	int                         { Token _ (TokenInt Decimal $$) }
	oct                         { Token _ (TokenInt Octal $$) }
	hex                         { Token _ (TokenInt Hex $$) }
	float                       { Token _ (TokenFloat $$) }
	string                      { Token _ (TokenString $$) }
	rune                        { Token _ (TokenRune $$) }
	raw                         { Token _ (TokenRaw $$) }

-- Precedences for operators. Lower is higher
%left '||' '&&' LOG
%left '==' '!=' '<=' '>' '>=' '<' COMP
%left '+' '-' '|' '^' ADD
%left '*' '/' '%' '<<' '>>' '&' '&^' MULT
%left UNARY


%%

Program
      : Package Alls                { Program $1 $2 }

Package
      : package id_raw ';'              { $2 }

Alls  : All Alls                    { $1 : $2 }
      | {- Empty -}                 { [] }

All   : Stmt                                                 { Stmt $1 }
      | func id_raw '(' ParamListEmpty ')' '{' Stmts '}' ';'         { Function (IdOrType $2) $4 Nothing $7 }
      | func id_raw '(' ParamListEmpty ')' Type '{' Stmts '}' ';'    { Function (IdOrType $2) $4 (Just $6) $8 }


ParamListEmpty : Param ',' ParamList               { $1 : $3 }

      | Param                             { [$1] }
      | {- Empty -}                       { [] }

ParamList 
      : Param ',' ParamList               { $1 : $3 }
      | Param                             { [$1] }

Param : InstantiationList Type            { Parameter $1 $2 }


Id    :: {Identifier}
      : id_raw				  { IdOrType $1 }	
      | id_raw ArrayAccess                { IdArray $1 $2 }
      | FieldList 	    	   	  { IdField $1 }                  

FieldList :: { [Identifier] }
      : IdField '.' FieldList  { $1 : $3 }
      | IdField '.' IdField    { $1 : [$3] }

IdField    :: { Identifier }
	   : id_raw			  { IdOrType $1 }
      	   | id_raw ArrayAccess 	  { IdArray $1 $2 }


ArrayAccess : '[' Expr ']' ArrayAccess { $2:$4 }
	    | '[' Expr ']'  	      { [$2] }

Stmts : Stmt Stmts                        { $1 : $2 }
      | {- Empty -}                       { [] }


Stmt  : var VarDec                                              { VarDec $2 }
      | var '(' VarDecList ')' ';'                              { VarDecList $3 }
      | type TypeDec                                            { TypeDec $2 }
      | type '(' TypeDecList ')' ';'                            { TypeDecList $3 }
      | return ';'                                              { Return Nothing }
      | return Expr ';'                                         { Return (Just $2) }
      | SimpleStmt ';'                                          { SimpleStmt $1 }
      | print '(' ExprListEmpty ')' ';'                         { Print $3 }
      | println '(' ExprListEmpty ')' ';'                       { Println $3 }
      | If                                                      { If $1 }
      | Switch ';'                                              { $1 }
      | for '{' Stmts '}' ';'                                   { Infinite $3 }
      | for Expr '{' Stmts '}' ';'                              { While $2 $4 }
      | for ';' ';' '{' Stmts '}' ';'				{ For Nothing Nothing Nothing $5 }
      | for SimpleStmt ';' ';' SimpleStmt '{' Stmts '}' ';'	{ For (Just $2) Nothing (Just $5) $7 }
      | for ';' ';' SimpleStmt '{' Stmts '}' ';'		{ For Nothing Nothing (Just $4) $6 }
      | for SimpleStmt ';' ';' '{' Stmts '}' ';'		{ For (Just $2) Nothing Nothing $6 }
      | for ';' Expr ';' '{' Stmts '}' ';'                      { For Nothing (Just $3) Nothing $6 }
      | for ';' Expr ';' SimpleStmt '{' Stmts '}' ';'           { For Nothing (Just $3) (Just $5) $7 }
      | for SimpleStmt ';' Expr ';' '{' Stmts '}' ';'             { For (Just $2) (Just $4) Nothing $7 }
      | for SimpleStmt ';' Expr ';' SimpleStmt '{' Stmts '}' ';'    { For (Just $2) (Just $4) (Just $6) $8 }
      | '{' Stmts '}' ';'	   	    	       	   	     	{ Block $2 }
      | break ';'                                                 { Break }
      | continue ';'                                            { Continue }

Switch
      : switch '{' ClauseList '}'                     { Switch Nothing Nothing $3 }
      | switch SimpleStmt ';' '{' ClauseList '}'      { Switch (Just $2) Nothing $5 }
      | switch Expr '{' ClauseList '}'                { Switch Nothing (Just $2) $4 }
      | switch SimpleStmt ';' Expr '{' ClauseList '}' { Switch (Just $2) (Just $4) $6 }

ClauseList
      :: { [Clause] }
      : Clause ClauseList             { $1 : $2 }
      | {- Empty -}                   { [] }
      
Clause
      :: { Clause }
      : case ExprList ':' Stmts       { Case $2 $4 }
      | default ':' Stmts             { Default $3 }

              
If    :: { IfStmt }
      : if SimpleStmt ';' Expr '{' Stmts '}' ';'                           { IfStmt (Just $2) $4 $6 Nothing }
      | if SimpleStmt ';' Expr '{' Stmts '}' else '{' Stmts '}' ';'      { IfStmt (Just $2) $4 $6 (Just (Right $10)) }
      | if SimpleStmt ';' Expr '{' Stmts '}' else If                  { IfStmt (Just $2) $4 $6 (Just (Left $9)) }
      | if Expr '{' Stmts '}' ';'                                  { IfStmt Nothing $2 $4 Nothing }
      | if Expr '{' Stmts '}' else '{' Stmts '}' ';'                  { IfStmt Nothing $2 $4 (Just (Right $8)) }
      | if Expr '{' Stmts '}' else If                             { IfStmt Nothing $2 $4 (Just (Left $7)) }

SimpleStmt
      : Expr                       { ExprStmt $1 }
      | Id '++'                    { Incr $1 }
      | Id '--'                    { Decr $1 }
      | VarList '=' ExprList       { Assign $1 $3 }
      | Id '+=' Expr               { ShortBinary PlusEq $1 $3 }
      | Id '-=' Expr               { ShortBinary MinusEq $1 $3 }
      | Id '*=' Expr               { ShortBinary MulEq $1 $3 }
      | Id '/=' Expr               { ShortBinary DivEq $1 $3 }
      | Id '%=' Expr               { ShortBinary ModEq $1 $3 }
      | Id '&=' Expr               { ShortBinary BitAndEq $1 $3 }
      | Id '|=' Expr               { ShortBinary BitOrEq $1 $3 }
      | Id '^=' Expr               { ShortBinary BitXorEq $1 $3 }
      | Id '<<=' Expr              { ShortBinary BitLShiftEq $1 $3 }
      | Id '>>=' Expr              { ShortBinary BitRShiftEq $1 $3 }
      | Id '&^=' Expr              { ShortBinary BitClearEq $1 $3 }
      | VarList ':=' ExprList      { ShortVarDec $1 $3 }

VarDec
      :: { Variable }
      : InstantiationList Type ';'                  { Variable $1 (Just $2) [] }
      | InstantiationList '=' ExprList  ';'         { Variable $1 Nothing $3 }
      | InstantiationList Type '=' ExprList ';'     { Variable $1 (Just $2) $4 }

VarDecList
      : VarDec VarDecList                 { $1 : $2 }
      | {- Empty -}                       { [] }

ExprList
      :: { [Expression] }
      : Expr ',' ExprList              { $1 : $3 }
      | Expr                           { [$1] }

ExprListEmpty
      :: { [Expression] }
      : Expr ',' ExprList               { $1 : $3 }
      | Expr                            { [$1] }
      | {- Empty -}                     { [] }

InstantiationList :: { [Identifier] }
      : id_raw ',' InstantiationList    { (IdOrType $1) : $3 }
      | id_raw                          { [(IdOrType $1)] }

VarList :: { [Identifier] }
      : Id ',' VarList              { $1 : $3 }
      | Id                          { [ $1 ] }

TypeDec ::  { TypeName }
      : id_raw Type ';'                    { TypeName (Type $1) $2 }

TypeDecList :: { [TypeName] }
      : TypeDec TypeDecList         { $1 : $2 }
      | {- Empty -}                 { [] }

StructListEmpty
      : InstantiationList Type ';' StructList { ($1, $2) : $4 }
      | InstantiationList Type ';'            { [($1, $2)] }
      | {- Empty -}                 { [] }

StructList
      : InstantiationList Type ';' StructList { ($1, $2) : $4 }
      | InstantiationList Type ';'            { [($1, $2)] }

Num   :: { Int }
      : int                         { $1 }
      | oct                         { $1 }
      | hex                         { $1 }

Lit   :: { Literal }
      : Num                         { Int' $1 }
      | float                       { Float64 $1 }
      | rune                        { Rune $1 }
      | string                      { String $1 }
      | raw                         { Raw $1 }

Expr  :: { Expression }
      : UnaryExpr      { $1 }
      | Expr '||' Expr %prec LOG { Binary Or $1 $3 }
      | Expr '&&' Expr %prec LOG { Binary And $1 $3 }
      | Expr RelOp Expr %prec COMP { Binary $2 $1 $3 }
      | Expr AddOp Expr %prec ADD { Binary $2 $1 $3 }
      | Expr MultOp Expr %prec MULT { Binary $2 $1 $3 }

UnaryExpr
      :: { Expression }
      : UnaryOp UnaryExpr { Unary $1 $2 }
      | PrimaryExpr { $1 }

UnaryOp	 : '+'	%prec UNARY { Pos }
	 | '-'  %prec UNARY { Neg }
	 | '!'  %prec UNARY { BoolNot }
	 | '^'	%prec UNARY { BitComplement }

PrimaryExpr
      :: { Expression }
      : '(' Expr ')'              { Brack $2 }
      | Id	                  { Id $1 }
      | Lit	                  { Literal $1 }
      | Id '(' ExprListEmpty ')'  { FuncCall  $1 $3 }
      | Append	                  { $1 }

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

MultOp	 : '*'		{ Mult }
	 | '/'		{ Div }
	 | '%'		{ Mod }
	 | '<<'		{ BitLShift }
	 | '>>'		{ BitRShift }
	 | '&'		{ BitAnd }
	 | '&^'		{ BitClear }


Type  :: { Type }
      : id_raw                          { Alias $1 }
      | '[' Num ']' Type           { Array $4 $2 }
      | '[' ']' Type                { Slice $3 }
      | struct '{' StructListEmpty '}'   { Struct $3 }


Append : append '(' Id ',' Expr ')' { Append $3 $5 }


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
