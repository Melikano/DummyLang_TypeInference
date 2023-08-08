module ClassDecInference where

import Dummy.Abs
import Types
import Unification

judgeClassDec :: [ClassDec] -> ClassEnv
judgeClassDec = foldr f []
  where
    f (Class_Dec tycs clsName tyVar ops) acc =
      Class
        { className = clsName,
          superClasses = map (\(TypeConstraint u _) -> u) tycs,
          typeVariable = tyVar,
          methods = map (\(ClassOp_Dec opName opType) -> (opName, \a -> sub opType (tyVar, a))) ops
        }
        : acc