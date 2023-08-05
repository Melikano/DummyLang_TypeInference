module TypeInference where

import Data.Char
import Data.List
import Dummy.Abs
import Dummy.Print
import Lib.Monads
import Types
import Unification
import Utils

restrictedLinearize ::
  [LIdent] -> [LIdent] -> [(SType, UIdent)] -> TyRel -> [Sub] -> [(String, String)] -> Either String ([LIdent], [(SType, UIdent)], [Sub], [(String, String)])
restrictedLinearize [] rbs cs tyrel subs dicts = return (rbs, cs, subs, dicts)
restrictedLinearize (b : bs) rbs cs tyrel subs dicts = do
  case lookup b subs of
    Nothing -> restrictedLinearize bs (b : rbs) cs tyrel subs dicts
    Just s -> do
      cSubs <- coalesce (b, s) subs
      case lookup (TVar_SType b) cs of
        Nothing -> restrictedLinearize bs rbs cs tyrel cSubs dicts
        Just (UIdent c) -> do
          let css = replace (TVar_SType b, UIdent c) (s, UIdent c) cs
          let reducedConstraints = applyTypeRels tyrel css
          restrictedLinearize bs rbs reducedConstraints tyrel cSubs (("dict" ++ c, "dict" ++ c ++ mshow s) : dicts)

getAllEqns ::
  [SolvedEqn] -> SolvedEqn
getAllEqns = foldr allEqns ([], [], [], [])
  where
    allEqns (f, b, c, s) (fs, bs, cs, ss) = (fs ++ f, b ++ bs, c ++ cs, unique (ss ++ s))

lookupSType :: SType -> [(SType, a)] -> Maybe a
lookupSType st [] = Nothing
lookupSType st ((s, a) : ss) | myEq st s = Just a | otherwise = lookupSType st ss

replaceTVar :: SType -> LIdent -> SType
replaceTVar (TVar_SType x) y = TVar_SType y
replaceTVar x _ = x

applyTypeRels :: TyRel -> [(SType, UIdent)] -> [(SType, UIdent)]
applyTypeRels (sc, sl) constraints =
  map
    ( \(s, u) -> case lookupSType s sc of
        Nothing -> (s, u)
        Just (TypeConstraint u' s') -> (replaceTVar s' (head (vars s)), u')
    )
    (filter (`notElem` sl) constraints)

procExist ::
  TyRel ->
  [LIdent] ->
  [TyC] ->
  [SolvedEqn] ->
  Either String ([LIdent], [LIdent], [(SType, UIdent)], [Sub], [(String, String)])
procExist tyrel bounds constraints eqns = do
  let parsedConstraints = map (\(TypeConstraint u l) -> (l, u)) constraints
  let (innerFrees, innerBounds, innerConstraints, innerSubs) = getAllEqns eqns
  let allBounds = bounds ++ innerBounds
  let allConstraints = parsedConstraints ++ innerConstraints
  (remainingBounds, remainingConstraints, remainingSubs, dicts) <-
    restrictedLinearize
      allBounds
      []
      allConstraints
      tyrel
      innerSubs
      []

  return
    ( filter (`notElem` allBounds) innerFrees,
      remainingBounds,
      remainingConstraints,
      remainingSubs,
      dicts
    )

procEqn :: (SType, SType) -> Either String SolvedEqn
procEqn (lt1, lt2) = do
  subs <- unify (lt1, lt2)
  return (vars lt1 ++ vars lt2, [], [], subs)

setCounter :: Int -> ExprInferer ()
setCounter counter = do
  modify (mapFst (const counter))

setContext :: Context -> ExprInferer ()
setContext context = do
  modify (mapSnd (const context))

newTypeVar :: ExprInferer LIdent
newTypeVar = do
  (counter, _) <- get
  setCounter (counter + 1)
  return (LIdent ("T" ++ show counter))

runJudge (Dummy_Prog classDecs instanceDecs exps) = do
  let cenv = judgeClassDec classDecs
  (ienv, ienvSel) <- sth cenv instanceDecs
  let insts = foldr renderInsts "" ienv
  (r, s) <- judgeExprs cenv ienv exps (0, [(LIdent "and", andType), (LIdent "head", headType)])
  let f = foldr gen [] (zip r (reverse [0 .. length r - 1]))
  let rss = map (\(e, t) -> printTree t ++ "; " ++ printTree e) f
  let g = foldr (\s acc -> s ++ "; " ++ acc) "" rss
  return (insts ++ ";" ++ g)

renderInsts (dictName, SimpDict exprs) acc = dictName ++ " = " ++ concatMap printTree exprs ++ "; " ++ acc
renderInsts (dictName, OvDict s exprs) acc = dictName ++ " = " ++ "\\" ++ s ++ " -> " ++ concatMap printTree exprs ++ "; " ++ acc

gen (((_, _, tycs, subs), expr, dicts), i) acc = (finalCode, finalType) : acc
  where
    finalCode = case expr of
      Ass_Expr l e -> Ass_Expr l (foldr (Abst_Expr . LIdent) e dicts)
      e' -> foldr (Abst_Expr . LIdent) e' dicts
    finalType = case lookup (LIdent $ "M" ++ "__" ++ show i) subs of
      Nothing -> DType_SType $ TVar_SType (LIdent $ "M" ++ "__" ++ show i)
      Just t -> DType_OvType $ OverLoadedType (map (\(s, u) -> TypeConstraint u s) tycs) t

judgeClassDec :: [ClassDec] -> CEnv
judgeClassDec = foldr f []
  where
    f (Class_Dec clsName tyVar ops) acc = (clsName, [TypeConstraint clsName (TVar_SType tyVar)], map (\(ClassOp_Dec opName opType) -> (opName, opType)) ops) : acc

andType :: SType
andType = Arrow_SType Bool_SType (Arrow_SType Bool_SType Bool_SType)

headType = Arrow_SType (List_SType (TVar_SType (LIdent "m"))) (TVar_SType (LIdent "m"))

tailType = Arrow_SType (List_SType (TVar_SType (LIdent "n"))) (List_SType (TVar_SType (LIdent "n")))

sth :: CEnv -> [InstDec] -> Either String (IEnv, [IEnvSel])
sth cenv = foldr f (Right ([], []))
  where
    f (Inst_Dec (UIdent u) s ops) i = do
      (ienv, ienvsels) <- i
      (r, _) <-
        judgeExprs
          cenv
          ( ( "dict" ++ u ++ mshow s,
              SimpDict []
            )
              : ienv
          )
          (map (\(ClassOp_Imp l expr) -> expr) ops)
          (0, [(LIdent "and", andType), (LIdent "head", headType), (LIdent "tail", tailType)])
      return
        ( ( "dict" ++ u ++ mshow s,
            SimpDict (map (\(_, e, _) -> e) r)
          )
            : ienv,
          zipWith (curry (\(ClassOp_Imp l expr, i) -> (l, \(SimpDict es) -> es !! i))) ops [0 .. length ops - 1] : ienvsels
        )
    f (Inst_Dec_With_Constraint tyc (UIdent u) s ops) i = do
      (ienv, ienvsels) <- i
      (r, _) <-
        judgeExprs
          cenv
          ( ( "dict" ++ u ++ mshow s,
              OvDict
                ( let (TypeConstraint (UIdent u) s) = tyc
                   in ("dict" ++ u ++ mshow s)
                )
                []
            )
              : ienv
          )
          (map (\(ClassOp_Imp l expr) -> expr) ops)
          (0, [(LIdent "and", andType), (LIdent "head", headType), (LIdent "tail", tailType)])
      return
        ( ( "dict" ++ u ++ mshow s,
            OvDict
              ( let (TypeConstraint (UIdent u) s) = tyc
                 in ("dict" ++ u ++ mshow s)
              )
              (map (\(_, e, _) -> e) r)
          )
            : ienv,
          zipWith (curry (\(ClassOp_Imp l expr, i) -> (l, \(SimpDict es) -> es !! i))) ops [0 .. length ops - 1] : ienvsels
        )

judgeInstDec :: [InstDec] -> TyRel
judgeInstDec = foldr f ([], [])
  where
    f (Inst_Dec u s ops) (cacc, acc) = (cacc, (s, u) : acc)
    f (Inst_Dec_With_Constraint tyc u s ops) (cacc, acc) = ((s, tyc) : cacc, acc)

judgeExprs cenv ienv exps initialState =
  foldl
    ( \(Right (j, s)) (e, i) -> do
        (newJ, newS) <- runStateT (judgeExpr cenv ienv e (LIdent ("M" ++ "__" ++ show i))) s
        return (newJ : j, newS)
    )
    (Right ([], initialState))
    (zip exps [0 .. length exps - 1])

include :: String -> String -> Bool
include xs ys = (any (isPrefixOf ys) . tails) $ xs

genCode :: Expr -> [(String, String)] -> [(String, Dictionary)] -> Expr
genCode (Var_Expr (LIdent x)) rep ienv = case lookup x rep of
  Nothing -> Var_Expr (LIdent x)
  Just e -> case lookup e ienv of
    Nothing -> Var_Expr (LIdent e)
    Just e' -> case e' of
      OvDict d _ -> App_Expr (Var_Expr (LIdent d)) (Var_Expr (LIdent e))
      _ -> Var_Expr (LIdent e)
genCode (App_Expr l r) rep ienv = App_Expr (genCode l rep ienv) (genCode r rep ienv)
genCode (Abst_Expr x t) rep ienv = Abst_Expr x (genCode t rep ienv)
genCode (LCase_Expr t Nil t0 (Cons a as) t1) rep ienv = LCase_Expr (genCode t rep ienv) Nil (genCode t0 rep ienv) (Cons a as) (genCode t1 rep ienv)
genCode e rep i = e

processDictionaries :: [(String, String)] -> [String] -> [(String, Dictionary)] -> ([(String, Dictionary)], [String])
processDictionaries dicts prevDicts ienv = (resolvedDicts, newDicts ++ remainingDicts ++ unresolved)
  where
    remainingDicts = filter (`notElem` map fst dicts) prevDicts
    (resolvedDicts, newDicts, unresolved) =
      foldr
        ( ( \rep (res, new, unres) -> case lookup rep ienv of
              Nothing -> (res, new, rep : unres)
              Just d' -> case d' of
                SimpDict _ -> ((rep, d') : res, new, unres)
                OvDict dict _ -> ((rep, d') : res, dict : new, unres)
          )
            . snd
        )
        ([], [], [])
        dicts

judgeExpr :: CEnv -> IEnv -> Expr -> LIdent -> ExprInferer (SolvedEqn, Expr, [String])
judgeExpr cenv ienv (Ass_Expr x t) qType = do
  (_, varCtx) <- get
  tType <- newTypeVar
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t tType
  newEqn <- lift $ procEqn (TVar_SType qType, TVar_SType tType)
  (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) [tType] [] [newEqn, tEqns]
  case lookup qType subs of
    Nothing -> throwError' "error"
    Just l -> setContext $ (x, l) : varCtx
  let (resolvedDicts, unresolvedDicts) = processDictionaries dicts innerDicts ienv
  return ((fs, bs, cs, subs), genCode (Ass_Expr x innerExpr) dicts resolvedDicts, unresolvedDicts)
judgeExpr cenv ienv (Var_Expr x) qType = do
  (_, varCtx) <- get
  let cls = filter (\(_, _, ops) -> any (\(x', s') -> x' == x) ops) cenv
  case cls of
    [] -> case lookup x varCtx of
      Nothing -> throwError' ("free " ++ show x)
      Just p -> do
        solvedEqn <- lift $ procEqn (TVar_SType qType, p)
        return (solvedEqn, Var_Expr x, [])
    [(UIdent u, tycs, ops)] -> case lookup x ops of
      Nothing -> let (LIdent x') = x in throwError' ("No Operation Definition" ++ x')
      Just s -> do
        newEqns <- lift $ procEqn (TVar_SType qType, s)
        (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) (vars s) tycs [newEqns]
        return
          ( (fs, bs, cs, subs),
            App_Expr (Var_Expr x) (Var_Expr (LIdent ("dict" ++ u))),
            ["dict" ++ u]
          )
    _ -> throwError' "redundant class operations"
judgeExpr cenv ienv (Abst_Expr x t) qType = do
  (_, varCtx) <- get
  xType <- newTypeVar
  tType <- newTypeVar
  setContext $ (x, TVar_SType xType) : varCtx
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t tType
  setContext varCtx
  newEqn <-
    lift $
      procEqn
        (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
  (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) [xType, tType] [] [newEqn, tEqns]
  let (resolvedDicts, unresolvedDicts) = processDictionaries dicts innerDicts ienv
  return ((fs, bs, cs, subs), genCode (Abst_Expr x innerExpr) dicts resolvedDicts, unresolvedDicts)
judgeExpr cenv ienv (App_Expr left right) qType = do
  lType <- newTypeVar
  rType <- newTypeVar
  (lEqns, lexpr, ldicts) <- judgeExpr cenv ienv left lType
  (rEqns, rexpr, rdicts) <- judgeExpr cenv ienv right rType
  newEqn <-
    lift $
      procEqn
        (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))
  (fs, bs, cs, subs, dicts) <- lift $ procExist ([], []) [rType, lType] [] (newEqn : lEqns : [rEqns])
  let (resolvedDicts, unresolvedDicts) = processDictionaries dicts (ldicts ++ rdicts) ienv
  return ((fs, bs, cs, subs), genCode (App_Expr lexpr rexpr) dicts resolvedDicts, unresolvedDicts)
judgeExpr cenv ienv (List_Expr Nil) qType = do
  aType <- newTypeVar
  newEqn <- lift $ procEqn (TVar_SType qType, List_SType (TVar_SType aType))
  (fs, bs, cs, subs, dicts) <-
    lift $
      procExist
        ([], [])
        [aType]
        []
        [newEqn]
  return
    ( (fs, bs, cs, subs),
      List_Expr Nil,
      []
    )
judgeExpr cenv ienv (List_Expr (Cons a as)) qType = do
  aType <- newTypeVar
  asType <- newTypeVar
  aeqn <- judgeExpr cenv ienv a aType
  aseqn <- judgeExpr cenv ienv as asType
  newEqn <-
    lift $
      procEqn
        ( TVar_SType qType,
          Arrow_SType
            (List_SType (TVar_SType aType))
            (List_SType (TVar_SType aType))
        )
  (fs, bs, cs, subs, dicts) <-
    lift $
      procExist
        ([], [])
        [aType]
        []
        [newEqn]
  return
    ( (fs, bs, cs, subs),
      List_Expr (Cons a as),
      []
    )
-- judgeExpr cenv ienv (LCase_Expr t Nil t0 (Cons a as) t1) qType = do
--   (counter, context) <- get
--   tType <- newTypeVar
--   t0Type <- newTypeVar
--   aType <- newTypeVar
--   asType <- newTypeVar
--   t1Type <- newTypeVar
--   xType <- newTypeVar
--   (tEqns, texpr, tdicts) <- judgeExpr cenv ienv t tType
--   (t0Eqns, t0expr, t0dicts) <- judgeExpr cenv ienv t0 t0Type
--   setContext ((a, TVar_SType aType) : (as, TVar_SType asType) : context)
--   (t1Eqns, t1expr, t1dicts) <- judgeExpr cenv ienv t1 t1Type
--   setContext context
--   newEqns <-
--     lift $
--       sequence
--         [ procEqn (TVar_SType tType, List_SType (TVar_SType xType)),
--           procEqn (TVar_SType t0Type, TVar_SType qType),
--           procEqn (TVar_SType aType, TVar_SType xType),
--           procEqn
--             ( TVar_SType asType,
--               List_SType (TVar_SType xType)
--             ),
--           procEqn (TVar_SType t1Type, TVar_SType qType)
--         ]
--   (fs, bs, cs, subs, dicts) <-
--     lift $
--       procExist
--         ([], [])
--         [xType, tType, aType, asType, t0Type, t1Type]
--         []
--         (newEqns ++ [tEqns] ++ [t0Eqns] ++ [t1Eqns])

--   let (resolvedDicts, unresolvedDicts) = processDictionaries dicts (tdicts ++ t0dicts ++ t1dicts) ienv
--   return ((fs, bs, cs, subs), genCode (LCase_Expr texpr Nil t0expr (Cons a as) t1expr) dicts resolvedDicts, unresolvedDicts)
judgeExpr cenv ienv (True_Expr t) qType = do
  newEqn <- lift $ procEqn (TVar_SType qType, Bool_SType)
  (fs, bs, cs, subs, dicts) <-
    lift $
      procExist
        ([], [])
        []
        []
        [newEqn]
  return
    ( (fs, bs, cs, subs),
      True_Expr t,
      []
    )
judgeExpr cenv ienv (False_Expr f) qType = do
  newEqn <- lift $ procEqn (TVar_SType qType, Bool_SType)
  (fs, bs, cs, subs, dicts) <-
    lift $
      procExist
        ([], [])
        []
        []
        [newEqn]
  return
    ( (fs, bs, cs, subs),
      False_Expr f,
      []
    )

-- testWithInst :: Prog
-- testWithInst =
--   Dummy_Prog
--     [ Class_Dec
--         (UIdent "Eq")
--         (LIdent "a")
--         [ ClassOp_Dec
--             (LIdent "equal")
--             (Arrow_SType (TVar_SType (LIdent "a")) (Arrow_SType (TVar_SType (LIdent "a")) Bool_SType))
--         ]
--     ]
--     [ Inst_Dec_With_Constraint
--         (TypeConstraint (UIdent "Eq") (TVar_SType (LIdent "kkk")))
--         (UIdent "Eq")
--         (List_SType (TVar_SType (LIdent "kkk")))
--         [ ClassOp_Imp
--             (LIdent "equal")
--             ( Abst_Expr
--                 (LIdent "xs")
--                 ( Abst_Expr
--                     (LIdent "ys")
--                     (True_Expr (DTrue "True"))
--                 )
--             )
--         ]
--     ]
--     [ Ass_Expr
--         (LIdent "tail")
--         ( Abst_Expr
--             (LIdent "xs")
--             ( LCase_Expr
--                 (Var_Expr (LIdent "xs"))
--                 Nil
--                 (List_Expr Nil)
--                 (Cons (LIdent "a") (LIdent "as"))
--                 (Var_Expr (LIdent "as"))
--             )
--         ),
--       Ass_Expr
--         (LIdent "f")
--         ( Abst_Expr
--             (LIdent "x")
--             ( Abst_Expr
--                 (LIdent "y")
--                 ( App_Expr
--                     ( App_Expr
--                         (Var_Expr (LIdent "equal"))
--                         (Var_Expr (LIdent "x"))
--                     )
--                     (App_Expr (Var_Expr (LIdent "tail")) (Var_Expr (LIdent "y")))
--                 )
--             )
--         )
--     ]

tt =
  Dummy_Prog [Class_Dec (UIdent "Eq") (LIdent "a") [ClassOp_Dec (LIdent "equal") (Arrow_SType (TVar_SType (LIdent "a")) (Arrow_SType (TVar_SType (LIdent "a")) Bool_SType))]] [Inst_Dec_With_Constraint (TypeConstraint (UIdent "Eq") (TVar_SType (LIdent "a"))) (UIdent "Eq") (List_SType (TVar_SType (LIdent "a"))) [ClassOp_Imp (LIdent "equal") (Abst_Expr (LIdent "xs") (Abst_Expr (LIdent "ys") (App_Expr (App_Expr (Var_Expr (LIdent "and")) (App_Expr (App_Expr (Var_Expr (LIdent "equal")) (App_Expr (Var_Expr (LIdent "head")) (Var_Expr (LIdent "xs")))) (App_Expr (Var_Expr (LIdent "head")) (Var_Expr (LIdent "ys"))))) (App_Expr (App_Expr (Var_Expr (LIdent "equal")) (App_Expr (Var_Expr (LIdent "tail")) (Var_Expr (LIdent "xs")))) (App_Expr (Var_Expr (LIdent "tail")) (Var_Expr (LIdent "ys")))))))],Inst_Dec (UIdent "Eq") Bool_SType [ClassOp_Imp (LIdent "equal") (Abst_Expr (LIdent "x") (Abst_Expr (LIdent "y") (True_Expr (DTrue "True"))))]] [Ass_Expr (LIdent "f") (App_Expr (App_Expr (Var_Expr (LIdent "equal")) (List_Expr (Cons (List_Expr (Cons (True_Expr (DTrue "True")) (List_Expr Nil))) (List_Expr Nil)))) (List_Expr (Cons (List_Expr (Cons (False_Expr (DFalse "False")) (List_Expr Nil))) (List_Expr Nil))))]