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
        Just c -> do
          let css = replace (TVar_SType b, c) (s, c) cs
          let reducedConstraints = applyTypeRels tyrel css
          restrictedLinearize bs rbs reducedConstraints tyrel cSubs (("dict" ++ show c ++ show b, "dict" ++ show c ++ mshow s) : dicts)

myEq :: SType -> SType -> Bool
myEq (TVar_SType _) (TVar_SType _) = Prelude.True
myEq (List_SType _) (List_SType _) = Prelude.True
myEq (TCons_SType _ _) (TCons_SType _ _) = Prelude.True
myEq (Arrow_SType _ _) (Arrow_SType _ _) = Prelude.True
myEq Bool_SType Bool_SType = Prelude.True
myEq _ _ = Prelude.False

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
  let esubs = unify (lt1, lt2)
  subs <- esubs
  return (vars lt1 ++ vars lt2, [], [], subs)

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
  (a, b) <- evalStateT (judgeExprs cenv ienv (head exps) (LIdent "Q")) (0, [(LIdent "tail", LIdent "TT0")])
  return a

-- (allres, allst) <-
--   foldl
--     (f cenv ienv)
--     (Right ((([], [], [], []), Var_Expr (LIdent "q"), []), (0, [])))
--     exps
-- return allst
-- where
--   f cenv ienv (Right (res, (cntr, ctx))) e =
--     evalStateT (judgeExprs cenv ienv e (LIdent ("Q" ++ show cntr))) (cntr, ctx)
--   f cenv ienv l e = l

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

include :: String -> String -> Bool
include xs ys = (any (isPrefixOf ys) . tails) $ xs

genCode :: Expr -> [(String, String)] -> Expr
genCode (App_Expr x (Var_Expr (LIdent y))) ienv = case lookup y ienv of
  Nothing -> App_Expr x (Var_Expr (LIdent y))
  Just d -> App_Expr x (Var_Expr (LIdent d))
genCode (App_Expr l r) ienv = App_Expr (genCode l ienv) (genCode r ienv)
genCode (Abst_Expr x t) ienv = Abst_Expr x (genCode t ienv)
genCode (LCase_Expr t Nil t0 (Cons a as) t1) ienv = LCase_Expr (genCode t ienv) Nil (genCode t0 ienv) (Cons a as) (genCode t1 ienv)
genCode e i = e

processDictionaries :: [(String, String)] -> [String] -> [(String, Dictionary)] -> ([(String, Dictionary)], [String])
processDictionaries dicts prevDicts ienv = (resolvedDicts, remainingDicts ++ unresolvedDicts)
  where
    remainingDicts = filter (`notElem` map fst dicts) prevDicts
    resolvedDicts =
      foldr
        ( ( \d acc -> case lookup d ienv of
              Nothing -> acc
              Just d' -> (d, d') : acc
          )
            . fst
        )
        []
        dicts
    unresolvedDicts = filter (`notElem` map fst resolvedDicts) (map snd dicts)

judgeExpr :: CEnv -> IEnv -> Expr -> LIdent -> ExprInferer (SolvedEqn, Expr, [String])
judgeExpr cenv ienv (Ass_Expr x t) qType = do
  (_, varCtx) <- get
  tType <- newTypeVar
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t qType
  setContext $ (x, tType) : varCtx
  case procEqn (TVar_SType qType, TVar_SType tType) of
    Left e -> throwError' e
    Right newEqn -> case procExist ([], []) [tType] [] [newEqn, tEqns] of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        let (resolvedDicts, unresolvedDicts) = processDictionaries dicts innerDicts ienv
         in return ((fs, bs, cs, subs), genCode (Ass_Expr x innerExpr) dicts, unresolvedDicts)
judgeExpr cenv ienv (Var_Expr x) qType = do
  (_, varCtx) <- get
  let cls = filter (\(_, _, ops) -> any (\(x', s') -> x' == x) ops) cenv
  case cls of
    [] -> case lookup x varCtx of
      Nothing -> throwError' ("free " ++ show x)
      Just p -> case procEqn (TVar_SType qType, TVar_SType p) of
        Left e -> throwError' e
        Right solvedEqn -> return (solvedEqn, Var_Expr x, [])
    [(UIdent u, tycs, ops)] -> case lookup x ops of
      Nothing -> throwError' "Error"
      Just s -> case procEqn (TVar_SType qType, s) of
        Left e -> throwError' e
        Right eq -> case procExist ([], []) (vars s) tycs [eq] of
          Left e -> throwError' e
          Right (fs, bs, cs, subs, dicts) ->
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
  setContext $ (x, xType) : varCtx
  (tEqns, innerExpr, innerDicts) <- judgeExpr cenv ienv t tType
  setContext varCtx
  let newEqn =
        procEqn
          (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
  case newEqn of
    Left e -> throwError' e
    Right eqn -> case procExist ([], []) [xType, tType] [] [eqn, tEqns] of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        let (resolvedDicts, unresolvedDicts) = processDictionaries dicts innerDicts ienv
         in return ((fs, bs, cs, subs), genCode (Abst_Expr x innerExpr) dicts, unresolvedDicts)
judgeExpr cenv ienv (App_Expr left right) qType = do
  lType <- newTypeVar
  rType <- newTypeVar
  (lEqns, lexpr, ldicts) <- judgeExpr cenv ienv left lType
  (rEqns, rexpr, rdicts) <- judgeExpr cenv ienv right rType
  let newEqn =
        procEqn
          (TVar_SType lType, Arrow_SType (TVar_SType rType) (TVar_SType qType))
  case newEqn of
    Left e -> throwError' e
    Right eqn -> case procExist ([], []) [rType, lType] [] (eqn : lEqns : [rEqns]) of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        let (resolvedDicts, unresolvedDicts) = processDictionaries dicts (ldicts ++ rdicts) ienv
         in return ((fs, bs, cs, subs), genCode (App_Expr lexpr rexpr) dicts, unresolvedDicts)
judgeExpr cenv ienv (List_Expr Nil) qType = do
  aType <- newTypeVar
  let newEqn = procEqn (TVar_SType qType, List_SType (TVar_SType aType))
  case newEqn of
    Left e -> throwError' e
    Right eqn -> case procExist
      ([], [])
      [aType]
      []
      [eqn] of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        return
          ( (fs, bs, cs, subs),
            List_Expr Nil,
            []
          )
judgeExpr cenv ienv (List_Expr (Cons a as)) qType = do
  aType <- newTypeVar
  let newEqn =
        procEqn
          ( TVar_SType qType,
            Arrow_SType
              (List_SType (TVar_SType aType))
              (List_SType (TVar_SType aType))
          )
  case newEqn of
    Left e -> throwError' e
    Right eqn -> case procExist
      ([], [])
      [aType]
      []
      [eqn] of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        return
          ( (fs, bs, cs, subs),
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
        foldr
          ( \e acc -> case e of
              Left x -> Left x
              Right y -> case acc of
                Left x' -> Left x'
                Right y' -> Right (y : y')
          )
          (Right [])
          [ procEqn (TVar_SType tType, List_SType (TVar_SType xType)),
            procEqn (TVar_SType t0Type, TVar_SType qType),
            procEqn (TVar_SType aType, TVar_SType xType),
            procEqn
              ( TVar_SType asType,
                List_SType (TVar_SType xType)
              ),
            procEqn (TVar_SType t1Type, TVar_SType qType)
          ]
  case newEqns of
    Left e -> throwError' e
    Right eqns -> case procExist
      ([], [])
      [xType, tType, aType, asType, t0Type, t1Type]
      []
      (eqns ++ [tEqns] ++ [t0Eqns] ++ [t1Eqns]) of
      Left e -> throwError' e
      Right (fs, bs, cs, subs, dicts) ->
        let (resolvedDicts, unresolvedDicts) = processDictionaries dicts (tdicts ++ t0dicts ++ t1dicts) ienv
         in return ((fs, bs, cs, subs), genCode (LCase_Expr texpr Nil t0expr (Cons a as) t1expr) dicts, unresolvedDicts)

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
    [ -- Ass_Expr
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