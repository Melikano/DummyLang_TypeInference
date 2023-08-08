module ExpressionInference where

import Dummy.Abs
import Lib.Monads
import Types
import Unification
import Utils

getAllEqns ::
  [SolvedEqn] -> SolvedEqn
getAllEqns = foldl allEqns ([], [], [], [])
  where
    allEqns (f, b, c, s) (fs, bs, cs, ss) = (fs !++! f, b !++! bs, c !++! cs, ss !++! s)

tycTotuple :: TyC -> (SType, String)
tycTotuple (TypeConstraint u s) = (s, u)

applyTypeRels :: TyRel -> [(SType, String)] -> [(SType, String)]
applyTypeRels tyrel =
  concatMap
    ( \(s, u) -> case lookup u tyrel of
        Nothing -> [(s, u)]
        Just cnstrnts ->
          concat
            ( foldr
                ( \(_, f) acc -> case f s of
                    Left e -> acc
                    Right tyc -> map tycTotuple tyc : acc
                )
                []
                cnstrnts
            )
    )

restrictedLinearize :: [String] -> [String] -> [Sub] -> [Sub] -> Either String ([String], [Sub], [Sub])
restrictedLinearize [] rbs rSubs apSubs = return (rbs, rSubs, apSubs)
restrictedLinearize (b : bs) rbs rSubs apSubs = do
  case lookup b rSubs of
    Nothing -> restrictedLinearize bs (b : rbs) rSubs apSubs
    Just s -> do
      cSubs <- coalesce (b, s) rSubs
      restrictedLinearize bs rbs cSubs ((b, s) : apSubs)

reduceConstraints :: TyRel -> [Sub] -> [(SType, String)] -> [(SType, String)]
reduceConstraints _ [] cs = cs
reduceConstraints tyrel ((b, s) : subs) cs =
  case lookup (TVar_SType b) cs of
    Nothing -> reduceConstraints tyrel subs cs
    Just c -> do
      let css = replace (TVar_SType b, c) (s, c) cs
      reduceConstraints tyrel subs css

solve :: TyRel -> TypeEqns -> Either String (SolvedEqn, [Sub])
solve tyrel (TypeExist bounds constraints solvedEqns) = do
  let (innerFrees, innerBounds, innerConstraints, innerSubs) = getAllEqns solvedEqns
  let parsedConstraints = map tycTotuple constraints
  let allBounds = bounds ++ innerBounds
  let allConstraints = parsedConstraints ++ innerConstraints
  (remainingBounds, remainingSubs, appliedSubs) <- restrictedLinearize allBounds [] innerSubs []
  let remainingConstraints = reduceConstraints tyrel appliedSubs allConstraints
  let finalConstraints = applyTypeRels tyrel remainingConstraints
  return ((filter (`notElem` allBounds) innerFrees, remainingBounds, finalConstraints, remainingSubs), appliedSubs)
solve tyrel (TypeEqn (lt1, lt2)) = do
  let esubs = unify (lt1, lt2)
  subs <- esubs
  return ((vars lt1 ++ vars lt2, [], [], subs), [])

-- function that sets counter in a state (Ryan's Tutorial notes)
setCounter :: Int -> ExprInferer ()
setCounter counter = do
  modify (mapFst (const counter))

-- function that sets context in a state
setContext :: Context -> ExprInferer ()
setContext context = do
  modify (mapSnd (const context))

-- function that generates a new type var using the counter (Ryan's Tutorial notes)
newTypeVar :: ExprInferer String
newTypeVar = do
  (counter, _) <- get
  setCounter (counter + 1)
  return ("T" ++ show counter)

findMethod :: ClassEnv -> String -> Either String (Maybe (String, [String], SType -> SType))
findMethod (Class {className, superClasses, methods} : cs) method =
  case filter (\(cm, _) -> cm == method) methods of
    [(cm, tm)] -> Right (Just (className, superClasses, tm))
    [] -> Right Nothing
    _ -> Left $ "redundant class method: " ++ method

updatePlaceholders :: [Sub] -> [Placeholder] -> [Placeholder]
updatePlaceholders [] ps = ps
updatePlaceholders ((b, s) : subs) ps = map (\(id, (ps, pt)) -> if pt == TVar_SType b then (id, (ps, s)) else (id, (ps, pt))) ps

judgeExpr :: ClassEnv -> TyRel -> DefnEnv -> Expr -> String -> ExprInferer (SolvedEqn, Expr, [Placeholder])
judgeExpr classEnv tyrel dEnv (Var_Expr x) qType = do
  case findMethod classEnv x of
    Left err -> lift $ Left err
    Right m -> case m of
      Nothing -> do
        case lookup x dEnv of
          Nothing -> do
            (_, context) <- get
            case lookup x context of
              Nothing -> lift $ Left ("free " ++ show x)
              Just p -> do
                (solvedEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, p))
                return (solvedEqn, Var_Expr x, [])
          Just (d, _) -> case d of
            DType_OvType (OverLoadedType tycs t) -> do
              (solvedInner, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, t))
              (solvedEqn, _) <- lift $ solve tyrel (TypeExist (unique . vars $ t) tycs [solvedInner])
              return (solvedEqn, Placeholder_Expr ("p_" ++ qType), [("p_" ++ qType, (x, TVar_SType qType))])
            DType_SType t -> do
              (solvedEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, t))
              return (solvedEqn, Var_Expr x, [])
      Just (className, superClasses, mtype) -> do
        tyVar <- newTypeVar
        let tvartype = TVar_SType tyVar
        (solvedInner, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, mtype tvartype))
        (solvedEqn, _) <-
          lift $
            solve
              tyrel
              ( TypeExist
                  (vars $ mtype tvartype)
                  (TypeConstraint className tvartype : map (`TypeConstraint` tvartype) superClasses)
                  [solvedInner]
              )
        return
          ( solvedEqn,
            Placeholder_Expr $ "p_" ++ tyVar,
            [("p_" ++ tyVar, (x, tvartype))]
          )
judgeExpr classEnv tyrel dEnv (Abst_Expr x t) qType = do
  (_, context) <- get
  xType <- newTypeVar
  tType <- newTypeVar
  setContext $ (x, TVar_SType xType) : context
  (tEqns, texpr, innerPlaceHolders) <- judgeExpr classEnv tyrel dEnv t tType
  setContext context
  (newEqn, _) <-
    lift $
      solve
        tyrel
        ( TypeEqn
            (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
        )
  (solvedEqn, appliedSubs) <- lift $ solve tyrel (TypeExist [xType, tType] [] [newEqn, tEqns])
  return (solvedEqn, Abst_Expr x texpr, updatePlaceholders appliedSubs innerPlaceHolders)
judgeExpr classEnv tyrel dEnv (App_Expr left right) qType = do
  lType <- newTypeVar
  rType <- newTypeVar
  (lEqns, lexpr, lph) <- judgeExpr classEnv tyrel dEnv left lType
  (rEqns, rexpr, rph) <- judgeExpr classEnv tyrel dEnv right rType
  (newEqn, _) <-
    lift $
      solve
        tyrel
        ( TypeEqn
            (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))
        )
  (solvedEqn, appliedSubs) <- lift $ solve tyrel (TypeExist [rType, lType] [] [newEqn, lEqns, rEqns])
  return (solvedEqn, App_Expr lexpr rexpr, updatePlaceholders appliedSubs (lph ++ rph))
judgeExpr classEnv tyrel dEnv (INT_Expr i) qType = do
  (newEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, Int_SType))
  return (newEqn, INT_Expr i, [])
judgeExpr classEnv tyrel dEnv (True_Expr t) qType = do
  (newEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, Bool_SType))
  return (newEqn, True_Expr t, [])
judgeExpr classEnv tyrel dEnv (False_Expr f) qType = do
  (newEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, Bool_SType))
  return (newEqn, False_Expr f, [])
judgeExpr classEnv tyrel dEnv (List_Expr l) qType = do
  lType <- newTypeVar
  inners <- mapM (\e -> judgeExpr classEnv tyrel dEnv e lType) l
  (newEqn, _) <- lift $ solve tyrel $ TypeEqn (TVar_SType qType, List_SType (TVar_SType lType))
  (solvedEqn, appliedSubs) <- lift $ solve tyrel (TypeExist [lType] [] (newEqn : map (\(s, _, _) -> s) inners))
  return (solvedEqn, List_Expr (map (\(_, e, _) -> e) inners), updatePlaceholders appliedSubs (concatMap (\(_, _, ps) -> ps) inners))
