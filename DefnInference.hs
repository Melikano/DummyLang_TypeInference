module DefnInference where
import Types
import Dummy.Abs
import Lib.Monads
import ExpressionInference
import Utils


judgeDefns :: ClassEnv -> TyRel -> [Defn] -> Either String DefnEnv
judgeDefns classEnv tyrel defns =
  foldl
    ( \(Right prevDefns) (Defn_Expr f t, i) -> do
        let dfnTyVarName = "D" ++ "__" ++ show i
        let newDefn = (f, (DType_OvType (OverLoadedType [] (TVar_SType dfnTyVarName)), t))
        ((_, _, cs, subs), e, placeHolders) <- evalStateT (judgeExpr classEnv tyrel (newDefn : prevDefns) t dfnTyVarName) (0, [])
        -- replacePlaceHolders e placeHolders
        traceMonad placeHolders
        case lookup dfnTyVarName subs of
          Nothing -> Left "error"
          Just stype ->
            if null cs
              then return ((f, (DType_SType stype, e)) : prevDefns)
              else return ((f, (DType_OvType (OverLoadedType (map (\(st, uc) -> TypeConstraint uc st) cs) stype), e)) : prevDefns)
    )
    (Right [])
    (zip defns [0 .. length defns - 1])