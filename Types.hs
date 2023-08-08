module Types where

import Dummy.Abs
import Lib.Monads

type ClassEnv = [Class]

data Class = Class
  { className :: String,
    typeVariable :: String,
    superClasses :: [String],
    methods :: [(String, SType -> SType)]
  }

type DefnEnv = [(String, (DType, Expr))]

data WithEqnExpr
  = Abst_WEExpr String WithEqnExpr TypeEqns
  | Var_WEExpr String TypeEqns
  | App_WEExpr WithEqnExpr WithEqnExpr TypeEqns
  | VarOV_WEExpr String SType TypeEqns
  | True_WEExpr True
  | False_WEExpr False

type ExprInferer a = StateT (Int, Context) (Either String) a

type Context = [(String, SType)]
type Placeholder = (String, (String, SType))
type Env = [(String, (String, TypeEqns))]

type Sub = (String, SType)

type TyRel = ([(SType, TyC)], [(SType, String)])

-- they can either be type existence with list of bound type vars and list of type equations or type equality between two type terms
data TypeEqns = TypeExist [String] [TyC] [SolvedEqn] | TypeEqn (SType, SType)
  deriving (Show)

type SolvedEqn = ([String], [String], [(SType, String)], [Sub])