module Types where

import Debug.Trace
import Dummy.Abs
import Lib.Monads

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

type ExprInferer a = StateT (Int, Context) (Either String) a

type Context = [(LIdent, SType)]

type Sub = (LIdent, SType)

type TyRel = ([(SType, TyC)], [(SType, UIdent)])

data Dictionary = SimpDict [Expr] | OvDict String [Expr] deriving (Show)

type IEnv = [(String, Dictionary)]

type IEnvSel = [(LIdent, Dictionary -> Expr)]

data TypeEqns = TypeExist [LIdent] [TyC] [TypeEqns] | TypeEqn (SType, SType)
  deriving (Show)

type SolvedEqn = ([LIdent], [LIdent], [(SType, UIdent)], [Sub])

type CEnv = [(UIdent, [TyC], [(LIdent, SType)])]
