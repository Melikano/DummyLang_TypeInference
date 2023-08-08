-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Dummy.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Dummy.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transTrue :: Dummy.Abs.True -> Result
transTrue x = case x of
  Dummy.Abs.True string -> failure x

transFalse :: Dummy.Abs.False -> Result
transFalse x = case x of
  Dummy.Abs.False string -> failure x

transProg :: Dummy.Abs.Prog -> Result
transProg x = case x of
  Dummy.Abs.Dummy_Prog classdecs instdecs defns -> failure x

transClassOpDec :: Dummy.Abs.ClassOpDec -> Result
transClassOpDec x = case x of
  Dummy.Abs.ClassOp_Dec string stype -> failure x

transClassOpImp :: Dummy.Abs.ClassOpImp -> Result
transClassOpImp x = case x of
  Dummy.Abs.ClassOp_Imp string expr -> failure x

transClassDec :: Dummy.Abs.ClassDec -> Result
transClassDec x = case x of
  Dummy.Abs.Class_Dec string1 string2 classopdecs -> failure x

transInstDec :: Dummy.Abs.InstDec -> Result
transInstDec x = case x of
  Dummy.Abs.Inst_Dec string stype classopimps -> failure x
  Dummy.Abs.Inst_Dec_With_Constraint tyc string stype classopimps -> failure x

transDefn :: Dummy.Abs.Defn -> Result
transDefn x = case x of
  Dummy.Abs.Defn_Expr string expr -> failure x

transExpr :: Dummy.Abs.Expr -> Result
transExpr x = case x of
  Dummy.Abs.Abst_Expr string expr -> failure x
  Dummy.Abs.Var_Expr string -> failure x
  Dummy.Abs.App_Expr expr1 expr2 -> failure x
  Dummy.Abs.True_Expr true -> failure x
  Dummy.Abs.False_Expr false -> failure x
  Dummy.Abs.Placeholder_Expr string -> failure x
  Dummy.Abs.List_Expr exprs -> failure x
  Dummy.Abs.INT_Expr integer -> failure x

transTyC :: Dummy.Abs.TyC -> Result
transTyC x = case x of
  Dummy.Abs.TypeConstraint string stype -> failure x

transOvType :: Dummy.Abs.OvType -> Result
transOvType x = case x of
  Dummy.Abs.OverLoadedType tycs stype -> failure x

transSType :: Dummy.Abs.SType -> Result
transSType x = case x of
  Dummy.Abs.TVar_SType string -> failure x
  Dummy.Abs.TCons_SType string1 string2 -> failure x
  Dummy.Abs.Bool_SType -> failure x
  Dummy.Abs.Arrow_SType stype1 stype2 -> failure x
  Dummy.Abs.List_SType stype -> failure x
  Dummy.Abs.Int_SType -> failure x

transDType :: Dummy.Abs.DType -> Result
transDType x = case x of
  Dummy.Abs.DType_OvType ovtype -> failure x
  Dummy.Abs.DType_SType stype -> failure x
