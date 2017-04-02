{-# LANGUAGE FlexibleInstances #-}

module CodeGen.Prefix
    (Prefixable(..)) where

import Language

class Prefixable a where
  prefix :: a -> a

  prefixList :: [a] -> [a]
  prefixList vals = map prefix vals

instance Prefixable Program where
    prefix (Program pckg all) = Program pckg (prefixList all)

instance Prefixable All where
    prefix (TopDec toplevel) = TopDec (prefix toplevel)
    prefix (Function fname params t stmts) = 
            Function (prefix fname) (prefixList params) (prefix t) (prefixList stmts)

instance Prefixable TopLevel where
    prefix (VarDec var) = VarDec (prefix var)
    prefix (VarDecList vars) = VarDecList (prefixList vars)
    prefix (TypeDec t) = TypeDec (prefix t)
    prefix (TypeDecList ts) = TypeDecList (prefixList ts)

instance Prefixable Variable where
    prefix (Variable ids t exprs) = 
            Variable (prefixList ids) (prefix t) (prefixList exprs)

instance Prefixable TypeName where
    prefix (TypeName t1 t2) = TypeName (prefix t1) (prefix t2)

instance Prefixable Identifier where
    prefix (IdOrType s) = IdOrType (prefix s)
    prefix (IdArray s exprs) = IdArray (prefix s) (prefixList exprs)
    prefix (IdField idents) = IdField (prefixList idents)

instance Prefixable Type where
    prefix (Alias s) = Alias (prefix s)
    prefix (Array t i) = Array (prefix t) i
    prefix (Slice t) = Slice (prefix t)
    prefix (Struct structs) = Struct (prefixList structs)
    prefix (Func ts t) = Func (prefixList ts) (prefix t)
    prefix other = other

instance Prefixable Parameter where
    prefix (Parameter ids t) = Parameter (prefixList ids) (prefix t)

instance Prefixable Stmt where
    prefix (StmtDec top) = StmtDec (prefix top)
    prefix (SimpleStmt simp) = SimpleStmt (prefix simp)
    prefix (Print exprs) = Print (prefixList exprs)
    prefix (Println exprs) = Println (prefixList exprs)
    prefix (Return expr) = Return (prefix expr)
    prefix (If ifstmt) = If (prefix ifstmt)
    prefix (Switch simp expr clauses) = 
            Switch (prefix simp) (prefix expr) (prefixList clauses)
    prefix (Infinite stmts) = Infinite (prefixList stmts)
    prefix (While expr stmts) = While (prefix expr) (prefixList stmts)
    prefix (For simp1 expr simp2 stmts) = 
            For (prefix simp1) (prefix expr) (prefix simp2) (prefixList stmts)
    prefix (Block stmts) = Block (prefixList stmts)
    prefix other = other

instance Prefixable SimpleStmt where
    prefix (StmtFuncCall fCall) = StmtFuncCall (prefix fCall)
    prefix (Incr ident) = Incr (prefix ident)
    prefix (Decr ident) = Decr (prefix ident)
    prefix (Assign ids exprs) = Assign (prefixList ids) (prefixList exprs)
    prefix (ShortBinary bop ident expr) = 
                ShortBinary bop (prefix ident) (prefix expr)
    prefix (ShortVarDec ids exprs) = 
            ShortVarDec (prefixList ids) (prefixList exprs)
    prefix other = other

instance Prefixable FunctionCall where 
    prefix (FunctionCall fname exprs) = 
                FunctionCall (prefix fname) (prefixList exprs)

instance Prefixable IfStmt where
    prefix (IfStmt simp expr stmts ifcont) = 
            IfStmt (prefix simp) (prefix expr) (prefixList stmts) (prefix ifcont)

instance Prefixable IfStmtCont where
    prefix (IfStmtCont (Just (Left ifstmt))) = 
            IfStmtCont (Just (Left (prefix ifstmt)))
    prefix (IfStmtCont (Just (Right stmts))) = 
            IfStmtCont (Just (Right (prefixList stmts)))
    prefix other = other

instance Prefixable Clause where
    prefix (Case exprs stmts) = Case (prefixList exprs) (prefixList stmts)
    prefix (Default stmts) = Default (prefixList stmts)

instance Prefixable Expression where
    prefix (Brack expr) = Brack (prefix expr)
    prefix (Id ident) = Id (prefix ident)
    prefix (ExprFuncCall fCall) = ExprFuncCall (prefix fCall)
    prefix (Append ident expr) = Append (prefix ident) (prefix expr)
    prefix (Unary u expr) = Unary u (prefix expr)
    prefix (Binary b expr1 expr2) = Binary b (prefix expr1) (prefix expr2)
    prefix other = other

instance Prefixable String where
    prefix "true" = "true"
    prefix "false" = "false"
    prefix s = "u_" ++ s

instance Prefixable (Maybe Type) where
    prefix (Just t) = Just (prefix t)
    prefix other = other

instance Prefixable (Maybe Expression) where
    prefix (Just t) = Just (prefix t)
    prefix other = other

instance Prefixable ([Identifier], Type) where
    prefix (ids, t) = ((prefixList ids), (prefix t))















