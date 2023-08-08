-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Dummy.

module Dummy.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Prog = Dummy_Prog [ClassDec] [InstDec] [Defn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClassOpDec = ClassOp_Dec String SType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClassOpImp = ClassOp_Imp String Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClassDec = Class_Dec String String [ClassOpDec]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data InstDec
    = Inst_Dec String SType [ClassOpImp]
    | Inst_Dec_With_Constraint TyC String SType [ClassOpImp]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Defn = Defn_Expr String Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = Abst_Expr String Expr
    | Var_Expr String
    | App_Expr Expr Expr
    | True_Expr True
    | False_Expr False
    | Placeholder_Expr String
    | List_Expr [Expr]
    | INT_Expr Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TyC = TypeConstraint String SType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OvType = OverLoadedType [TyC] SType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SType
    = TVar_SType String
    | TCons_SType String String
    | Bool_SType
    | Arrow_SType SType SType
    | List_SType SType
    | Int_SType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DType = DType_OvType OvType | DType_SType SType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype True = True String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype False = False String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

