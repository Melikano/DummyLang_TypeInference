module Tests where

import Dummy.Abs
import Prelude hiding (True)

test1 = Dummy_Prog [Class_Dec [] "Eq" "a" [ClassOp_Dec "equal" (Arrow_SType (TVar_SType "a") (Arrow_SType (TVar_SType "a") Bool_SType))]] [Inst_Dec_With_Constraint [TypeConstraint "Eq" (TVar_SType "a")] "Eq" (List_SType (TVar_SType "a")) [ClassOp_Imp "equal" (Abst_Expr "xs" (Abst_Expr "ys" (True_Expr (True "True"))))], Inst_Dec "Eq" (TVar_SType "Integer") [ClassOp_Imp "equal" (Abst_Expr "x" (Abst_Expr "y" (True_Expr (True "True"))))]] [Defn_Expr "f" (App_Expr (App_Expr (Var_Expr "equal") (List_Expr [List_Expr [INT_Expr 2]])) (List_Expr [List_Expr [INT_Expr 3]]))]