-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Dummy.Par
  ( happyError
  , myLexer
  , pProg
  , pListClassDec
  , pListInstDec
  , pListExpr
  , pClassOpDec
  , pClassOpImp
  , pClassDec
  , pInstDec
  , pListClassOpDec
  , pListClassOpImp
  , pList
  , pExpr
  , pTyC
  , pOvType
  , pListTyC
  , pSType
  , pDType
  ) where

import Prelude

import qualified Dummy.Abs
import Dummy.Lex

}

%name pProg Prog
%name pListClassDec ListClassDec
%name pListInstDec ListInstDec
%name pListExpr ListExpr
%name pClassOpDec ClassOpDec
%name pClassOpImp ClassOpImp
%name pClassDec ClassDec
%name pInstDec InstDec
%name pListClassOpDec ListClassOpDec
%name pListClassOpImp ListClassOpImp
%name pList List
%name pExpr Expr
%name pTyC TyC
%name pOvType OvType
%name pListTyC ListTyC
%name pSType SType
%name pDType DType
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  ','        { PT _ (TS _ 1)      }
  '->'       { PT _ (TS _ 2)      }
  ':'        { PT _ (TS _ 3)      }
  ';'        { PT _ (TS _ 4)      }
  '<'        { PT _ (TS _ 5)      }
  '='        { PT _ (TS _ 6)      }
  '=>'       { PT _ (TS _ 7)      }
  '>'        { PT _ (TS _ 8)      }
  'Bool'     { PT _ (TS _ 9)      }
  '['        { PT _ (TS _ 10)     }
  '[]'       { PT _ (TS _ 11)     }
  '\\'       { PT _ (TS _ 12)     }
  ']'        { PT _ (TS _ 13)     }
  'case'     { PT _ (TS _ 14)     }
  'class'    { PT _ (TS _ 15)     }
  'instance' { PT _ (TS _ 16)     }
  'of'       { PT _ (TS _ 17)     }
  'where'    { PT _ (TS _ 18)     }
  L_True     { PT _ (T_True $$)   }
  L_False    { PT _ (T_False $$)  }
  L_UIdent   { PT _ (T_UIdent $$) }
  L_LIdent   { PT _ (T_LIdent $$) }

%%

True :: { Dummy.Abs.True }
True  : L_True { Dummy.Abs.True $1 }

False :: { Dummy.Abs.False }
False  : L_False { Dummy.Abs.False $1 }

UIdent :: { Dummy.Abs.UIdent }
UIdent  : L_UIdent { Dummy.Abs.UIdent $1 }

LIdent :: { Dummy.Abs.LIdent }
LIdent  : L_LIdent { Dummy.Abs.LIdent $1 }

Prog :: { Dummy.Abs.Prog }
Prog
  : ListClassDec ListInstDec ListExpr { Dummy.Abs.Dummy_Prog $1 $2 $3 }

ListClassDec :: { [Dummy.Abs.ClassDec] }
ListClassDec
  : {- empty -} { [] }
  | ClassDec { (:[]) $1 }
  | ClassDec ';' ListClassDec { (:) $1 $3 }

ListInstDec :: { [Dummy.Abs.InstDec] }
ListInstDec
  : {- empty -} { [] }
  | InstDec { (:[]) $1 }
  | InstDec ';' ListInstDec { (:) $1 $3 }

ListExpr :: { [Dummy.Abs.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ';' ListExpr { (:) $1 $3 }

ClassOpDec :: { Dummy.Abs.ClassOpDec }
ClassOpDec : LIdent ':' SType { Dummy.Abs.ClassOp_Dec $1 $3 }

ClassOpImp :: { Dummy.Abs.ClassOpImp }
ClassOpImp : LIdent '=' Expr { Dummy.Abs.ClassOp_Imp $1 $3 }

ClassDec :: { Dummy.Abs.ClassDec }
ClassDec
  : 'class' UIdent LIdent 'where' ListClassOpDec { Dummy.Abs.Class_Dec $2 $3 $5 }

InstDec :: { Dummy.Abs.InstDec }
InstDec
  : 'instance' UIdent SType 'where' ListClassOpImp { Dummy.Abs.Inst_Dec $2 $3 $5 }
  | 'instance' '<' TyC '>' '=>' UIdent SType 'where' ListClassOpImp { Dummy.Abs.Inst_Dec_With_Constraint $3 $6 $7 $9 }

ListClassOpDec :: { [Dummy.Abs.ClassOpDec] }
ListClassOpDec
  : {- empty -} { [] } | ClassOpDec ListClassOpDec { (:) $1 $2 }

ListClassOpImp :: { [Dummy.Abs.ClassOpImp] }
ListClassOpImp
  : {- empty -} { [] } | ClassOpImp ListClassOpImp { (:) $1 $2 }

List :: { Dummy.Abs.List }
List
  : '[]' { Dummy.Abs.Nil }
  | LIdent ':' LIdent { Dummy.Abs.Cons $1 $3 }

Expr :: { Dummy.Abs.Expr }
Expr
  : LIdent '=' Expr { Dummy.Abs.Ass_Expr $1 $3 }
  | '\\' LIdent '->' Expr { Dummy.Abs.Abst_Expr $2 $4 }
  | LIdent { Dummy.Abs.Var_Expr $1 }
  | Expr Expr { Dummy.Abs.App_Expr $1 $2 }
  | List { Dummy.Abs.List_Expr $1 }
  | 'case' Expr 'of' List '->' Expr ';' List '->' Expr { Dummy.Abs.LCase_Expr $2 $4 $6 $8 $10 }
  | True { Dummy.Abs.True_Expr $1 }
  | False { Dummy.Abs.False_Expr $1 }

TyC :: { Dummy.Abs.TyC }
TyC : UIdent SType { Dummy.Abs.TypeConstraint $1 $2 }

OvType :: { Dummy.Abs.OvType }
OvType
  : '<' ListTyC '>' '=>' SType { Dummy.Abs.OverLoadedType $2 $5 }

ListTyC :: { [Dummy.Abs.TyC] }
ListTyC
  : {- empty -} { [] }
  | TyC { (:[]) $1 }
  | TyC ',' ListTyC { (:) $1 $3 }

SType :: { Dummy.Abs.SType }
SType
  : LIdent { Dummy.Abs.TVar_SType $1 }
  | UIdent LIdent { Dummy.Abs.TCons_SType $1 $2 }
  | 'Bool' { Dummy.Abs.Bool_SType }
  | SType '->' SType { Dummy.Abs.Arrow_SType $1 $3 }
  | '[' SType ']' { Dummy.Abs.List_SType $2 }

DType :: { Dummy.Abs.DType }
DType
  : OvType { Dummy.Abs.DType_OvType $1 }
  | SType { Dummy.Abs.DType_SType $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

