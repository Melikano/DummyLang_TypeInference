module TypeInference where

import Dummy.Abs
import Dummy.Print
import Lib.Monads
import Types
import Unification
import Utils

restrictedLinearize ::
  [LIdent] -> [LIdent] -> [(SType, UIdent)] -> TyRel -> [Sub] -> Either String ([LIdent], [(SType, UIdent)], [Sub])
restrictedLinearize [] rbs cs tyrel subs = return (rbs, cs, subs)
restrictedLinearize (b : bs) rbs cs tyrel subs = do
  case lookup b subs of
    Nothing -> restrictedLinearize bs (b : rbs) cs tyrel subs
    Just s -> do
      cSubs <- coalesce (b, s) subs
      case lookup (TVar_SType b) cs of
        Nothing -> restrictedLinearize bs rbs cs tyrel cSubs
        Just c -> do
          let css = replace (TVar_SType b, c) (s, c) cs
          let reducedConstraints = applyTypeRels tyrel css
          restrictedLinearize bs rbs reducedConstraints tyrel cSubs

myEq :: SType -> SType -> Bool
myEq (TVar_SType _) (TVar_SType _) = Prelude.True
myEq (List_SType _) (List_SType _) = Prelude.True
myEq (TCons_SType _ _) (TCons_SType _ _) = Prelude.True
myEq (Arrow_SType _ _) (Arrow_SType _ _) = Prelude.True
myEq Bool_SType Bool_SType = Prelude.True
myEq _ _ = Prelude.False

getAllEqns ::
  [Either String SolvedEqn] -> Either String SolvedEqn
getAllEqns = foldr allEqns (Right ([], [], [], []))
  where
    allEqns newEitherFBS accEitherFBS = do
      (f, b, c, s) <- newEitherFBS
      (fs, bs, cs, ss) <- accEitherFBS
      return (fs ++ f, b ++ bs, c ++ cs, unique (ss ++ s))

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
  [Either String SolvedEqn] ->
  Either String ([LIdent], [LIdent], [(SType, UIdent)], [Sub])
procExist tyrel bounds constraints eqns = do
  let parsedConstraints = map (\(TypeConstraint u l) -> (l, u)) constraints
  (innerFrees, innerBounds, innerConstraints, innerSubs) <- getAllEqns eqns
  let allBounds = bounds ++ innerBounds
  let allConstraints = parsedConstraints ++ innerConstraints
  (remainingBounds, remainingConstraints, remainingSubs) <-
    restrictedLinearize
      allBounds
      []
      allConstraints
      tyrel
      innerSubs
  return
    ( filter (`notElem` allBounds) innerFrees,
      remainingBounds,
      remainingConstraints,
      remainingSubs
    )

procEqn :: (SType, SType) -> Either String SolvedEqn
procEqn (lt1, lt2) = do
  let esubs = unify (lt1, lt2)
  subs <- esubs
  return (vars lt1 ++ vars lt2, [], [], subs)

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
--         ( LIdent "tail",
--           ( LIdent "TE",
--             TypeExist
--               [LIdent "TEE"]
--               []
--               [ TypeEqn
--                   ( TVar_SType (LIdent "TE"),
--                     Arrow_SType (List_SType (TVar_SType (LIdent "TEE"))) (List_SType (TVar_SType (LIdent "TEE")))
--                   )
--               ]
--           )
--         )
--           : env
--       )
--   (f, b, cs, subs) <- solveEquations tyrel result
-- (finalConstraints, finalSubs) <- emptyFrees (filter (/= LIdent "Q") f) b cs subs
-- case lookup "Q" (map (\(LIdent x, s) -> (x, s)) subs) of
--   Nothing -> Right (DType_SType (TVar_SType (LIdent "T0")))
--   Just s -> case cs of
--     [] -> Right (DType_SType s)
--     cs -> Right (DType_OvType (OverLoadedType (map (\(l, u) -> TypeConstraint u l) cs) s))

-- call restricted linearize on free vars to get rid of them
-- emptyFrees :: [LIdent] -> [LIdent] -> [(SType, UIdent)] -> [Sub] -> Either String ([(SType, UIdent)], [Sub])
-- emptyFrees [] bs cs subs = return (cs, subs)
-- emptyFrees frees bs cs subs = do
--   (f, css, ss) <- restrictedLinearize frees bs cs subs
--   return (css, ss)

-- function that sets counter in a state (Ryan's Tutorial notes)
setCounter :: Int -> ExprInferer ()
setCounter counter = do
  modify (mapFst (const counter))

-- function that sets context in a state
setContext :: Context -> ExprInferer ()
setContext context = do
  modify (mapSnd (const context))

-- function that generates a new type var using the counter (Ryan's Tutorial notes)
newTypeVar :: ExprInferer LIdent
newTypeVar = do
  (counter, _) <- get
  setCounter (counter + 1)
  return (LIdent ("T" ++ show counter))

-- a function for running type judgement
-- runJudge :: Prog -> Either String (Int, Context)
runJudge (Dummy_Prog classDecs instanceDecs exps) = do
  let cenv = judgeClassDec classDecs
  let (ienv, _) = sth instanceDecs
  -- (a, b) <- evalStateT (judgeExprs cenv ienv (head exps) (LIdent "Q")) (0,[(LIdent "tail", LIdent "TT0")])
  -- return a
  (allres, allst) <-
    foldl
      (f cenv ienv)
      (Right ((Right ([], [], [], []), Var_Expr (LIdent "q"), []), (0, [])))
      exps
  return allst 
  where
    f cenv ienv  (Right (res, (cntr, ctx))) e =
      evalStateT (judgeExprs cenv ienv e (LIdent ("Q" ++ show cntr))) (cntr,ctx)
    f cenv ienv l e = l

-- (finalConstraints, finalSubs) <- emptyFrees (filter (/= LIdent "Q") f) b c subs
-- return finalSubs

judgeClassDec :: [ClassDec] -> CEnv
judgeClassDec = foldr f []
  where
    f (Class_Dec clsName tyVar ops) acc = (clsName, [TypeConstraint clsName (TVar_SType tyVar)], map (\(ClassOp_Dec opName opType) -> (opName, opType)) ops) : acc

sth :: [InstDec] -> (IEnv, [IEnvSel])
sth = foldr f ([], [])
  where
    f (Inst_Dec (UIdent u) s ops) (ienv, ienvsels) =
      ( ( "dict" ++ u ++ mshow s,
          SimpDict
            ( map
                (\(ClassOp_Imp l expr) -> expr)
                ops
            )
        )
          : ienv,
        zipWith (curry (\(ClassOp_Imp l expr, i) -> (l, \(SimpDict es) -> es !! i))) ops [0 .. length ops - 1] : ienvsels
      )
    f (Inst_Dec_With_Constraint tyc (UIdent u) s ops) (ienv, ienvsels) =
      ( ( "dict" ++ u ++ mshow s,
          OvDict
            ( \dict ->
                map
                  (\(ClassOp_Imp l expr) -> expr)
                  ops
            )
        )
          : ienv,
        zipWith (curry (\(ClassOp_Imp l expr, i) -> (l, \(SimpDict es) -> es !! i))) ops [0 .. length ops - 1] : ienvsels
      )

judgeInstDec :: [InstDec] -> TyRel
judgeInstDec = foldr f ([], [])
  where
    f (Inst_Dec u s ops) (cacc, acc) = (cacc, (s, u) : acc)
    f (Inst_Dec_With_Constraint tyc u s ops) (cacc, acc) = ((s, tyc) : cacc, acc)

judgeExprs cenv ienv exp qType = do
  result <- judgeExpr cenv ienv exp qType
  state <- get
  return (result, state)

judgeExpr :: CEnv -> IEnv -> Expr -> LIdent -> ExprInferer (Either String SolvedEqn, Expr, [String])
judgeExpr cenv ienv (Ass_Expr x t) qType = do
  (_, varCtx) <- get
  tType <- newTypeVar
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t qType
  setContext $ (x, tType) : varCtx
  let newEqn = procEqn (TVar_SType qType, TVar_SType tType)
  return (procExist ([], []) [tType] [] [newEqn, tEqns], Ass_Expr x innerExpr, innerDicts)
judgeExpr cenv ienv (Var_Expr x) qType = do
  (_, varCtx) <- get
  let cls = filter (\(_, _, ops) -> any (\(x', s') -> x' == x) ops) cenv
  case cls of
    [] -> case lookup x varCtx of
      Nothing -> throwError' ("free " ++ show x)
      Just p -> return (procEqn (TVar_SType qType, TVar_SType p), Var_Expr x, [])
    [(UIdent u, tycs, ops)] -> case lookup x ops of
      Nothing -> throwError' "Error"
      Just s ->
        return
          ( procExist ([], []) (vars s) tycs [procEqn (TVar_SType qType, s)],
            App_Expr (Var_Expr x) (Var_Expr (LIdent ("dict" ++ u))),
            ["dict" ++ u]
          )
    _ -> throwError' "redundant class operations"
judgeExpr cenv ienv (Abst_Expr x t) qType = do
  (_, varCtx) <- get
  xType <- newTypeVar
  tType <- newTypeVar
  setContext $ (x, xType) : varCtx
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t tType
  setContext varCtx
  let newEqn =
        procEqn
          (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
  return (procExist ([], []) [xType, tType] [] [newEqn, tEqns], Abst_Expr x innerExpr, innerDicts)
judgeExpr cenv ienv (App_Expr left right) qType = do
  lType <- newTypeVar
  rType <- newTypeVar
  (lEqns, lexpr, ldicts) <- judgeExpr cenv ienv left lType
  (rEqns, rexpr, rdicts) <- judgeExpr cenv ienv right rType
  let newEqn =
        procEqn
          (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))
  return (procExist ([], []) [rType, lType] [] (newEqn : lEqns : [rEqns]), App_Expr lexpr rexpr, ldicts ++ rdicts)
judgeExpr cenv ienv (List_Expr Nil) qType = do
  aType <- newTypeVar
  return
    ( procExist
        ([], [])
        [aType]
        []
        [procEqn (TVar_SType qType, List_SType (TVar_SType aType))],
      List_Expr Nil,
      []
    )
judgeExpr cenv ienv (List_Expr (Cons a as)) qType = do
  aType <- newTypeVar
  return
    ( procExist
        ([], [])
        [aType]
        []
        [ procEqn
            ( TVar_SType qType,
              Arrow_SType
                (List_SType (TVar_SType aType))
                (List_SType (TVar_SType aType))
            )
        ],
      List_Expr (Cons a as),
      []
    )
judgeExpr cenv ienv (LCase_Expr t Nil t0 (Cons a as) t1) qType = do
  (counter, context) <- get
  tType <- newTypeVar
  t0Type <- newTypeVar
  aType <- newTypeVar
  asType <- newTypeVar
  t1Type <- newTypeVar
  xType <- newTypeVar
  (tEqns, texpr, tdicts) <- judgeExpr cenv ienv t tType
  (t0Eqns, t0expr, t0dicts) <- judgeExpr cenv ienv t0 t0Type
  setContext ((a, aType) : (as, asType) : context)
  (t1Eqns, t1expr, t1dicts) <- judgeExpr cenv ienv t1 t1Type
  setContext context
  let newEqns =
        [ procEqn (TVar_SType tType, List_SType (TVar_SType xType)),
          procEqn (TVar_SType t0Type, TVar_SType qType),
          procEqn (TVar_SType aType, TVar_SType xType),
          procEqn
            ( TVar_SType asType,
              List_SType (TVar_SType xType)
            ),
          procEqn (TVar_SType t1Type, TVar_SType qType)
        ]
  return
    ( procExist
        ([], [])
        [xType, tType, aType, asType, t0Type, t1Type]
        []
        (newEqns ++ [tEqns] ++ [t0Eqns] ++ [t1Eqns]),
      LCase_Expr texpr Nil t0expr (Cons a as) t1expr,
      tdicts ++ t0dicts ++ t1dicts
    )

-- test =
--   Dummy_Prog
--     [ Class_Dec
--         (UIdent "Eq")
--         (LIdent "a")
--         [ClassOp_Dec (LIdent "equal") (Arrow_SType (TVar_SType (LIdent "a")) (Arrow_SType (TVar_SType (LIdent "a")) Bool_SType))]
--     ]
--     []
--     [Ass_Expr (LIdent "f") (Abst_Expr (LIdent "x") (Abst_Expr (LIdent "y") (App_Expr (App_Expr (Var_Expr (LIdent "equal")) (Var_Expr (LIdent "x"))) (Var_Expr (LIdent "y")))))]

testWithInst :: Prog
testWithInst =
  Dummy_Prog
    [ Class_Dec
        (UIdent "Eq")
        (LIdent "a")
        [ ClassOp_Dec
            (LIdent "equal")
            (Arrow_SType (TVar_SType (LIdent "a")) (Arrow_SType (TVar_SType (LIdent "a")) Bool_SType))
        ]
    ]
    [ Inst_Dec_With_Constraint
        (TypeConstraint (UIdent "Eq") (TVar_SType (LIdent "kkk")))
        (UIdent "Eq")
        (List_SType (TVar_SType (LIdent "kkk")))
        [ ClassOp_Imp
            (LIdent "equal")
            ( Abst_Expr
                (LIdent "xs")
                ( Abst_Expr
                    (LIdent "ys")
                    (True_Expr (Dummy.Abs.True "True"))
                )
            )
        ]
    ]
    [ 
      -- Ass_Expr
      --   (LIdent "tail")
      --   ( Abst_Expr
      --       (LIdent "xs")
      --       ( LCase_Expr
      --           (Var_Expr (LIdent "xs"))
      --           Nil
      --           (List_Expr Nil)
      --           (Cons (LIdent "a") (LIdent "as"))
      --           (Var_Expr (LIdent "as"))
      --       )
      --   ),
      Ass_Expr
        (LIdent "f")
        ( Abst_Expr
            (LIdent "x")
            ( Abst_Expr
                (LIdent "y")
                ( App_Expr
                    ( App_Expr
                        (Var_Expr (LIdent "equal"))
                        (Var_Expr (LIdent "x"))
                    )
                    (App_Expr (Var_Expr (LIdent "tail")) (Var_Expr (LIdent "y")))
                )
            )
        )
    ]

-- insts :: [InstDec]
-- insts =
--   [ Inst_Dec_With_Constraint
--       (TypeConstraint (UIdent "Eq") (TVar_SType (LIdent "kkk")))
--       (UIdent "Eq")
--       (List_SType (TVar_SType (LIdent "kkk")))
--       [ ClassOp_Imp
--           (LIdent "equal")
--           ( Abst_Expr
--               (LIdent "xs")
--               ( Abst_Expr
--                   (LIdent "ys")
--                   (True_Expr (Dummy.Abs.True "True"))
--               )
--           )
--       ]
--   ]