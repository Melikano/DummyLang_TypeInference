module TypeInference where

import Debug.Trace
import Dummy.Abs
import Dummy.Print
import Lib.Monads
import Types
import Unification

throwError' :: String -> StateT s (Either String) a
throwError' error = lift (Left error)

-- fold function for typeEqn type
-- for TypeEqn constructor -> just apply g type equality
-- for TypeExist constructor -> apply f on list of bound variables and map fold on inner list of type eqns
-- foldTypeEqns ::
--   ([String] -> [TyC] -> [b] -> b) -> ((SType, SType) -> b) -> TypeEqns -> b
-- foldTypeEqns f g (TypeEqn eq) = g eq
-- foldTypeEqns f g (TypeExist l cs eqs) = f l cs (map (foldTypeEqns f g) eqs)

-- linearize function restricted to input list of vars
-- takes each bound variable, finds the substitution for it (if exist)
-- applies the substitution and does the matching if needed (coalesce function)
-- recursively calls for the rest of bound variables
-- if there is no substitutions for a bound variable then it returns it as the list of not-substituted vars
-- so they can be passed up in the main algorithm
restrictedLinearize :: [String] -> [String] -> [Sub] -> [Sub] -> Either String ([String], [Sub], [Sub])
restrictedLinearize [] rbs rSubs apSubs = return (rbs, rSubs, apSubs)
restrictedLinearize (b : bs) rbs rSubs apSubs = do
  case lookup b rSubs of
    Nothing -> restrictedLinearize bs (b : rbs) rSubs apSubs
    Just s -> do
      cSubs <- coalesce (b, s) rSubs
      restrictedLinearize bs rbs cSubs ((b, s) : apSubs)

-- case lookup (TVar_SType b) cs of
--   Nothing -> restrictedLinearize tyrel bs rbs cs cSubs
--   Just c -> do
--     let css = replace (TVar_SType b, c) (s, c) cs
--     let reducedConstraints = applyTypeRels tyrel css
--     restrictedLinearize tyrel bs rbs reducedConstraints cSubs

replace :: (SType, String) -> (SType, String) -> [(SType, String)] -> [(SType, String)]
replace a1 a2 as = a2 : filter (/= a1) as

myEq :: SType -> SType -> Bool
myEq (TVar_SType _) (TVar_SType _) = Prelude.True
myEq (List_SType _) (List_SType _) = Prelude.True
myEq (TCons_SType _ _) (TCons_SType _ _) = Prelude.True
myEq (Arrow_SType _ _) (Arrow_SType _ _) = Prelude.True
myEq Bool_SType Bool_SType = Prelude.True
myEq _ _ = Prelude.False

-- function that gathers all free variables, bound variables and substitutions of a list of equations into a
-- triple of form ([frees], [bounds], [subs]) of type SolvedEqns
getAllEqns ::
  [SolvedEqn] -> SolvedEqn
getAllEqns = foldl allEqns ([], [], [], [])
  where
    allEqns (f, b, c, s) (fs, bs, cs, ss) = (fs !++! f, b !++! bs, c !++! cs, ss !++! s)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = if x `elem` xs then unique xs else x : unique xs

(!++!) :: Eq a => [a] -> [a] -> [a]
(!++!) = (++) . unique

lookupSType :: SType -> [(SType, a)] -> Maybe a
lookupSType st [] = Nothing
lookupSType st ((s, a) : ss) | myEq st s = Just a | otherwise = lookupSType st ss

replaceTVar :: SType -> String -> SType
replaceTVar (TVar_SType x) y = TVar_SType y
replaceTVar x _ = x

applyTypeRels :: TyRel -> [(SType, String)] -> [(SType, String)]
applyTypeRels (sc, sl) constraints =
  map
    ( \(s, u) -> case lookupSType s sc of
        Nothing -> (s, u)
        Just (TypeConstraint u' s') -> case vars s of
          [] -> (s, u)
          v : vs -> (replaceTVar s' v, u')
    )
    (filter (`notElem` sl) constraints)

-- solveEquations :: TyRel -> WithEqnExpr -> Either String SolvedEqn
-- solveEquations = undefined

reduceConstraints :: TyRel -> [Sub] -> [(SType, String)] -> [(SType, String)]
reduceConstraints _ _ [] = []
reduceConstraints _ [] cs = cs
reduceConstraints tyrel ((b, s) : subs) cs =
  case lookup (TVar_SType b) cs of
    Nothing -> reduceConstraints tyrel subs cs
    Just c -> do
      let css = replace (TVar_SType b, c) (s, c) cs
      let reducedConstraints = applyTypeRels tyrel css
      reduceConstraints tyrel subs reducedConstraints

solve :: TyRel -> TypeEqns -> Either String (SolvedEqn, [Sub])
solve tyrel (TypeExist bounds constraints solvedEqns) = do
  let (innerFrees, innerBounds, innerConstraints, innerSubs) = getAllEqns solvedEqns
  let parsedConstraints = map (\(TypeConstraint u l) -> (l, u)) constraints
  let allBounds = bounds ++ innerBounds
  let allConstraints = parsedConstraints ++ innerConstraints
  (remainingBounds, remainingSubs, appliedSubs) <- restrictedLinearize allBounds [] innerSubs []
  let remainingConstraints = reduceConstraints tyrel appliedSubs allConstraints
  return ((filter (`notElem` allBounds) innerFrees, remainingBounds, remainingConstraints, remainingSubs), appliedSubs)
solve tyrel (TypeEqn (lt1, lt2)) = do
  let esubs = unify (lt1, lt2)
  subs <- esubs
  return ((vars lt1 ++ vars lt2, [], [], subs), [])

-- solveEquationsHelper :: TyRel -> WithEqnExpr -> Either String (Expr, SolvedEqn)
-- solveEquationsHelper tyrel (Var_WEExpr x eqn) = do
--   solvedEqn <- solve tyrel [] eqn
--   return (Var_Expr x, solvedEqn)
-- solveEquationsHelper tyrel (VarOV_WEExpr x t eqn) = do
--   solvedEqn <- solve tyrel [] eqn
--   return (VarOV_Expr x t, solvedEqn)
-- solveEquationsHelper tyrel (Abst_WEExpr x e eqn) = do
--   (innerExpr, innerSolved) <- solveEquationsHelper tyrel e
--   solvedEqns <- solve tyrel [innerSolved] eqn
--   return (Abst_Expr x innerExpr, solvedEqns)
-- solveEquationsHelper tyrel (App_WEExpr l r eqn) = do
--   (innerL, innerSolvedL) <- solveEquationsHelper tyrel l
--   (innerR, innerSolvedR) <- solveEquationsHelper tyrel r
--   solvedEqns <- solve tyrel [innerSolvedL, innerSolvedR] eqn
--   return (App_Expr innerL innerR, solvedEqns)
-- solveEquationsHelper tyrel (List_WEExpr l eqn) = do
--   solvedEqn <- solve tyrel [] eqn
--   return (List_Expr l, solvedEqn)
-- solveEquationsHelper tyrel (LCase_WEExpr t Nil t0 (Cons a as) t1 eqn) = do
--   (innerT, innerSolvedT) <- solveEquationsHelper tyrel t
--   (innerT0, innerSolvedT0) <- solveEquationsHelper tyrel t0
--   (innerT1, innerSolvedT1) <- solveEquationsHelper tyrel t1
--   solvedEqn <- solve tyrel [innerSolvedT, innerSolvedT0, innerSolvedT1] eqn
--   return (LCase_Expr innerT Nil innerT0 (Cons a as) innerT1, solvedEqn)
-- solveEquationsHelper tyrel (True_WEExpr t) = Right (True_Expr t, ([], [], [], []))
-- solveEquationsHelper tyrel (False_WEExpr t) = Right (False_Expr t, ([], [], [], []))

-- foldTypeEqns (procExist tyrel) procEqn eqns

-- procExist ::
--   TyRel ->
--   [String] ->
--   [TyC] ->
--   [Either String SolvedEqn] ->
--   Either String SolvedEqn

-- procExist tyrel bounds constraints eqns = do
--   let parsedConstraints = map (\(TypeConstraint u l) -> (l, u)) constraints
--   (innerFrees, innerBounds, innerConstraints, innerSubs, innerExps) <- getAllEqns eqns
--   let allBounds = bounds ++ innerBounds
--   let allConstraints = parsedConstraints ++ innerConstraints
--   (remainingBounds, remainingConstraints, remainingSubs, remianingOvs) <-
--     restrictedLinearize
--       allBounds
--       []
--       allConstraints
--       tyrel
--       innerSubs
--       innerExps
--   return (filter (`notElem` allBounds) innerFrees, remainingBounds, remainingConstraints, remainingSubs, remianingOvs)

-- procEqn (lt1, lt2) = do
--   let esubs = unify (lt1, lt2)
--   subs <- esubs
--   return (vars lt1 ++ vars lt2, [], [], subs)

-- first get the equations by running judgements
-- then solve the equations to get list of free vars, list of bound vars and list of subs
-- linearize free vars and remaining subs to empty all frees
-- the sub for Q (which we passed as the total type) is the answer of type inference
-- typeInference :: Prog -> Either String DType
-- typeInference (Dummy_Prog classDecs instanceDecs exps) = do
--   let Class_Dec name tyVar ops = head classDecs
--   let env = judgeClassDec ops 0 name tyVar
--   let tyrel = judgeInstDec instanceDecs
--   -- return tyrel
--   result <-
--     evalStateT
--       (judgeExpr (head exps) (LIdent "Q"))
--       ( 0,
--         [],
--         (  "tail",
--           (  "TE",
--             TypeExist
--               [ "TEE"]
--               []
--               [ TypeEqn
--                   ( TVar_SType ( "TE"),
--                     Arrow_SType (List_SType (TVar_SType (LIdent "TEE"))) (List_SType (TVar_SType (LIdent "TEE")))
--                   )
--               ]
--           )
--         )
--           : env
--       )
--   (f, b, cs, subs) <- solveEquations tyrel result
--   -- (finalConstraints, finalSubs) <- emptyFrees (filter (/= LIdent "Q") f) b cs subs
--   case lookup "Q" (map (\(x, s) -> (x, s)) subs) of
--     Nothing -> Right (DType_SType (TVar_SType ("T0")))
--     Just s -> case cs of
--       [] -> Right (DType_SType s)
--       cs -> Right (DType_OvType (OverLoadedType (map (\(l, u) -> TypeConstraint u l) cs) s))

-- call restricted linearize on free vars to get rid of them
-- emptyFrees :: [LIdent] -> [LIdent] -> [(SType, String)] -> [Sub] -> Either String ([(SType, String)], [Sub])
-- emptyFrees [] bs cs subs = return (cs, subs)
-- emptyFrees frees bs cs subs = do
--   (f, css, ss) <- restrictedLinearize frees bs cs subs
--   return (css, ss)

mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (x, y) = (f x, y)

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

-- a function for running type judgement
runJudge :: Prog -> Either String DefnEnv
runJudge (Dummy_Prog classDecs instanceDecs exps) = do
  let cenv = judgeClassDec classDecs
  let tyrel = judgeInstDec instanceDecs
  traceMonad tyrel
  judgeDefns cenv tyrel exps

-- let insts = foldr renderInsts "" ienv
-- (r, s) <- judgeExprs cenv ienv exps (0, [(LIdent "and", andType), (LIdent "head", headType)])
-- let f = foldr gen [] (zip r (reverse [0 .. length r - 1]))
-- let rss = map (\(e, t) -> printTree t ++ "; " ++ printTree e) f
-- let g = foldr (\s acc -> s ++ "; " ++ acc) "" rss
-- return (insts ++ ";" ++ g)
updatePlaceholders :: [Sub] -> [Placeholder] -> [Placeholder]
updatePlaceholders [] ps = ps
updatePlaceholders ((b, s) : subs) ps = map (\(id, (ps, pt)) -> if pt == TVar_SType b then (id, (ps, s)) else (id, (ps, pt))) ps

judgeClassDec :: [ClassDec] -> ClassEnv
judgeClassDec = foldr f []
  where
    f (Class_Dec clsName tyVar ops) acc =
      Class
        { className = clsName,
          superClasses = [],
          typeVariable = tyVar,
          methods = map (\(ClassOp_Dec opName opType) -> (opName, \a -> sub opType (tyVar, a))) ops
        }
        : acc

judgeInstDec :: [InstDec] -> TyRel
judgeInstDec = foldr f ([], [])
  where
    f (Inst_Dec u s ops) (cacc, acc) = (cacc, (s, u) : acc)
    f (Inst_Dec_With_Constraint tyc u s ops) (cacc, acc) = ((s, tyc) : cacc, acc)

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

findMethod :: ClassEnv -> String -> Either String (Maybe (String, [String], SType -> SType))
findMethod (Class {className, superClasses, methods} : cs) method =
  case filter (\(cm, _) -> cm == method) methods of
    [(cm, tm)] -> Right (Just (className, superClasses, tm))
    [] -> Right Nothing
    _ -> Left $ "redundant class method: " ++ method

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

judgeExpr :: ClassEnv -> TyRel -> DefnEnv -> Expr -> String -> ExprInferer (SolvedEqn, Expr, [Placeholder])
judgeExpr classEnv tyrel dEnv (Var_Expr x) qType = do
  case findMethod classEnv x of
    Left err -> throwError' err
    Right m -> case m of
      Nothing -> do
        case lookup x dEnv of
          Nothing -> do
            (_, context) <- get
            case lookup x context of
              Nothing -> throwError' ("free " ++ show x)
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

-- judgeExpr classEnv tyrel dEnv (List_Expr Nil) qType = do
--   aType <- newTypeVar
--   (newEqn, _) <- lift $ solve tyrel (TypeEqn (TVar_SType qType, List_SType (TVar_SType aType)))
--   (solvedEqn, _) <-
--     lift $
--       solve
--         tyrel
--         ( TypeExist
--             [aType]
--             []
--             [newEqn]
--         )
--   return
--     ( solvedEqn,
--       List_Expr
--         Nil,
--       []
--     )
-- judgeExpr cenv tyrel dEnv (List_Expr (Cons a as)) qType = do
--   aType <- newTypeVar
--   asType <- newTypeVar
--   aeqn <- judgeExpr cenv tyrel dEnv a aType
--   aseqn <- judgeExpr cenv ienv as asType
--   newEqn <-
--     lift $
--       procEqn
--         ( TVar_SType qType,
--           Arrow_SType
--             (List_SType (TVar_SType aType))
--             (List_SType (TVar_SType aType))
--         )
--   (fs, bs, cs, subs, dicts) <-
--     lift $
--       procExist
--         ([], [])
--         [aType]
--         []
--         [newEqn]
--   return
--     ( (fs, bs, cs, subs),
--       List_Expr (Cons a as),
--       []
--     )
-- judgeExpr cenv (Var_Expr x) qType = do
--   (_, varCtx) <- get
--   let cls = filter (\(_, _, ops) -> any (\(x', s') -> x' == x) ops) cenv
--   case cls of
--     [] -> case lookup x varCtx of
--       Nothing -> throwError' ("free " ++ show x)
--       Just p -> do
--         solvedEqn <- lift $ procEqn (TVar_SType qType, p)
--         return (solvedEqn, Var_Expr x, [])
--     [(UIdent u, tycs, ops)] -> case lookup x ops of
--       Nothing -> let (LIdent x') = x in throwError' ("No Operation Definition" ++ x')
--       Just s -> do
--         newEqns <- lift $ procEqn (TVar_SType qType, s)
--         (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) (vars s) tycs [newEqns]
--         return
--           ( (fs, bs, cs, subs),
--             App_Expr (Var_Expr x) (Var_Expr (LIdent ("dict" ++ u))),
--             ["dict" ++ u]
--           )
--     _ -> throwError' "redundant class operations"
-- judgeExpr cenv ienv (Abst_Expr x t) qType = do
--   (_, varCtx) <- get
--   xType <- newTypeVar
--   tType <- newTypeVar
--   setContext $ (x, TVar_SType xType) : varCtx
--   (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t tType
--   setContext varCtx
--   newEqn <-
--     lift $
--       procEqn
--         (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
--   (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) [xType, tType] [] [newEqn, tEqns]
--   let (resolvedDicts, unresolvedDicts) = processDictionaries dicts innerDicts ienv
--   return ((fs, bs, cs, subs), genCode (Abst_Expr x innerExpr) dicts resolvedDicts, unresolvedDicts)
-- judgeExpr cenv ienv (App_Expr left right) qType = do
--   lType <- newTypeVar
--   rType <- newTypeVar
--   (lEqns, lexpr, ldicts) <- judgeExpr cenv ienv left lType
--   (rEqns, rexpr, rdicts) <- judgeExpr cenv ienv right rType
--   newEqn <-
--     lift $
--       procEqn
--         (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))
--   (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) [rType, lType] [] (newEqn : lEqns : [rEqns])
--   let (resolvedDicts, unresolvedDicts) = processDictionaries dicts (ldicts ++ rdicts) ienv
--   return ((fs, bs, cs, subs), genCode (App_Expr lexpr rexpr) dicts resolvedDicts, unresolvedDicts)
-- judgeExpr cenv ienv (List_Expr Nil) qType = do
--   aType <- newTypeVar
--   newEqn <- lift $ procEqn (TVar_SType qType, List_SType (TVar_SType aType))
--   (fs, bs, cs, subs, dicts) <-
--     lift $
--       procExist
--         ([], [])
--         [aType]
--         []
--         [newEqn]
--   return
--     ( (fs, bs, cs, subs),
--       List_Expr Nil,
--       []
--     )
-- judgeExpr cenv ienv (List_Expr (Cons a as)) qType = do
--   aType <- newTypeVar
--   asType <- newTypeVar
--   aeqn <- judgeExpr cenv ienv a aType
--   aseqn <- judgeExpr cenv ienv as asType
--   newEqn <-
--     lift $
--       procEqn
--         ( TVar_SType qType,
--           Arrow_SType
--             (List_SType (TVar_SType aType))
--             (List_SType (TVar_SType aType))
--         )
--   (fs, bs, cs, subs, dicts) <-
--     lift $
--       procExist
--         ([], [])
--         [aType]
--         []
--         [newEqn]
--   return
--     ( (fs, bs, cs, subs),
--       List_Expr (Cons a as),
--       []
--     )

-- judgeExpr :: ClassEnv -> DefnEnv -> Expr -> String -> Judger WithEqnExpr
-- judgeExpr classEnv dEnv (Var_Expr x) qType = do
--   case findMethod classEnv x of
--     Left err -> throwError' err
--     Right m -> case m of
--       Nothing -> do
--         case lookup x dEnv of
--           Nothing -> do
--             (_, context) <- get
--             case lookup x context of
--               Nothing -> throwError' ("free " ++ show x)
--               Just p -> return (Var_WEExpr x (TypeEqn (TVar_SType qType, p)))
--           Just (d, _) -> case d of
--             DType_OvType (OverLoadedType tycs t) -> return (VarOV_WEExpr x (TVar_SType qType) (TypeExist (unique . vars $ t) tycs [TypeEqn (TVar_SType qType, t)]))
--             DType_SType t -> return (Var_WEExpr x (TypeEqn (TVar_SType qType, t)))
--       Just (tyVar, constraints, mtype) ->
--         return
--           ( VarOV_WEExpr
--               x
--               (TVar_SType tyVar)
--               ( TypeExist
--                   (vars mtype)
--                   constraints
--                   [TypeEqn (TVar_SType qType, mtype)]
--               )
--           )
-- judgeExpr classEnv dEnv (Abst_Expr x t) qType = do
--   (_, context) <- get
--   xType <- newTypeVar
--   tType <- newTypeVar
--   setContext $ (x, TVar_SType xType) : context
--   tEqns <- judgeExpr classEnv dEnv t tType
--   setContext context
--   let newEqn =
--         TypeEqn
--           (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))

--   return (Abst_WEExpr x tEqns (TypeExist [xType, tType] [] [newEqn]))
-- judgeExpr classEnv dEnv (App_Expr left right) qType = do
--   lType <- newTypeVar
--   rType <- newTypeVar
--   lEqns <- judgeExpr classEnv dEnv left lType
--   rEqns <- judgeExpr classEnv dEnv right rType
--   let newEqn =
--         TypeEqn
--           (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))

--   return (App_WEExpr lEqns rEqns (TypeExist [rType, lType] [] [newEqn]))
-- judgeExpr classEnv dEnv (List_Expr Nil) qType = do
--   aType <- newTypeVar
--   return
--     ( List_WEExpr
--         Nil
--         ( TypeExist
--             [aType]
--             []
--             [TypeEqn (TVar_SType qType, List_SType (TVar_SType aType))]
--         )
--     )
-- judgeExpr classEnv dEnv (List_Expr (Cons a as)) qType = do
--   aType <- newTypeVar
--   return
--     ( List_WEExpr
--         (Cons a as)
--         ( TypeExist
--             [aType]
--             []
--             [ TypeEqn
--                 ( TVar_SType qType,
--                   Arrow_SType
--                     (List_SType (TVar_SType aType))
--                     (List_SType (TVar_SType aType))
--                 )
--             ]
--         )
--     )
-- judgeExpr classEnv dEnv (LCase_Expr t Nil t0 (Cons a as) t1) qType = do
--   (counter, context) <- get
--   tType <- newTypeVar
--   t0Type <- newTypeVar
--   aType <- newTypeVar
--   asType <- newTypeVar
--   t1Type <- newTypeVar
--   xType <- newTypeVar
--   tEqns <- judgeExpr classEnv dEnv t tType
--   t0Eqns <- judgeExpr classEnv dEnv t0 t0Type
--   setContext ((a, TVar_SType aType) : (as, TVar_SType asType) : context)
--   t1Eqns <- judgeExpr classEnv dEnv t1 t1Type
--   setContext context
--   let newEqns =
--         [ TypeEqn (TVar_SType tType, List_SType (TVar_SType xType)),
--           TypeEqn (TVar_SType t0Type, TVar_SType qType),
--           TypeEqn (TVar_SType aType, TVar_SType xType),
--           TypeEqn
--             ( TVar_SType asType,
--               List_SType (TVar_SType xType)
--             ),
--           TypeEqn (TVar_SType t1Type, TVar_SType qType)
--         ]
--   return
--     ( LCase_WEExpr
--         tEqns
--         Nil
--         t0Eqns
--         (Cons a as)
--         t1Eqns
--         ( TypeExist
--             [xType, tType, aType, asType, t0Type, t1Type]
--             []
--             newEqns
--         )
--     )

test =
  Dummy_Prog
    [ Class_Dec
        "Eq"
        "a"
        [ClassOp_Dec "equal" (Arrow_SType (TVar_SType "a") (Arrow_SType (TVar_SType "a") Bool_SType))]
    ]
    []
    [Defn_Expr "f" (Abst_Expr "x" (Abst_Expr "y" (App_Expr (App_Expr (Var_Expr "equal") (Var_Expr "x")) (Var_Expr "y"))))]

tt =
  Dummy_Prog
    [ Class_Dec
        "Eq"
        "a"
        [ClassOp_Dec "equal" (Arrow_SType (TVar_SType "a") (Arrow_SType (TVar_SType "a") Bool_SType))]
    ]
    [ Inst_Dec_With_Constraint
        (TypeConstraint "Eq" (TVar_SType "a"))
        "Eq"
        (List_SType (TVar_SType "a"))
        [ClassOp_Imp "equal" (Abst_Expr "xs" (Abst_Expr "ys" (True_Expr (Dummy.Abs.True "True"))))],
      Inst_Dec "Eq" (TVar_SType "Integer") [ClassOp_Imp "equal" (Abst_Expr "x" (Abst_Expr "y" (True_Expr (Dummy.Abs.True "True"))))]
    ]
    [Defn_Expr "f" (App_Expr (App_Expr (Var_Expr "equal") (List_Expr [List_Expr [INT_Expr 2]])) (List_Expr [List_Expr [INT_Expr 3]]))]