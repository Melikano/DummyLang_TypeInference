module Types where
import Dummy.Abs
import Lib.Monads

type ExprInferer a = StateT (Int, Context) (Either String) a

type Context = [(LIdent, LIdent)]

type Sub = (LIdent, SType)

type TyRel = ([(SType, TyC)], [(SType, UIdent)])

data Dictionary = SimpDict [Expr] | OvDict (Dictionary -> [Expr])
instance Show Dictionary where
    show (SimpDict exprs) = "<" ++ foldr (\e acc -> show e ++ ", " ++ acc ) "" exprs ++ ">"
    show (OvDict f) = "ovdict" 

type IEnv = [(String, Dictionary)]

type IEnvSel = [(LIdent, Dictionary -> Expr)]

data TypeEqns = TypeExist [LIdent] [TyC] [TypeEqns] | TypeEqn (SType, SType)
  deriving (Show)

type SolvedEqn = ([LIdent], [LIdent], [(SType, UIdent)], [Sub])

type CEnv = [(UIdent, [TyC], [(LIdent, SType)])]

