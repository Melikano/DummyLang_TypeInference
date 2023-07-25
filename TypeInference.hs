module TypeInference where

import Dummy.Abs
import Dummy.Print
import Lib.Monads

throwError' :: String -> StateT s (Either String) a
throwError' error = lift (Left error)

type ExprInferer a = StateT (Int, Context) (Either String) a

type Context = [(LIdent, LIdent)]

type Sub = (LIdent, SType)

type TyRel = ([(SType, TyC)], [(SType, UIdent)])

-- data type for type equations:
-- they can either be type existence with list of bound type vars and list of type equations or type equality between two type terms
data TypeEqns = TypeExist [LIdent] [TyC] [TypeEqns] | TypeEqn (SType, SType)
  deriving (Show)

type SolvedEqn = ([LIdent], [LIdent], [(SType, UIdent)], [Sub])

-- fold function for typeEqn type
-- for TypeEqn constructor -> just apply g type equality
-- for TypeExist constructor -> apply f on list of bound variables and map fold on inner list of type eqns
foldTypeEqns ::
  ([LIdent] -> [TyC] -> [b] -> b) -> ((SType, SType) -> b) -> TypeEqns -> b
foldTypeEqns f g (TypeEqn eq) = g eq
foldTypeEqns f g (TypeExist l cs eqs) = f l cs (map (foldTypeEqns f g) eqs)

-- linearize function restricted to input list of vars
-- takes each bound variable, finds the substitution for it (if exist)
-- applies the substitution and does the matching if needed (coalesce function)
-- recursively calls for the rest of bound variables
-- if there is no substitutions for a bound variable then it returns it as the list of not-substituted vars
-- so they can be passed up in the main algorithm
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

replace :: (SType, UIdent) -> (SType, UIdent) -> [(SType, UIdent)] -> [(SType, UIdent)]
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
  [Either String SolvedEqn] -> Either String SolvedEqn
getAllEqns = foldr allEqns (Right ([], [], [], []))
  where
    allEqns newEitherFBS accEitherFBS = do
      (f, b, c, s) <- newEitherFBS
      (fs, bs, cs, ss) <- accEitherFBS
      -- frees = union of all frees
      -- bounds = disjoint union of bounds (as they must be distinct)
      -- subs = first subs appended onto the second
      return (fs ++ f, b ++ bs, c ++ cs, unique (ss ++ s))

unique [] = []
unique (x : xs) = if x `elem` xs then unique xs else x : (unique xs)

-- removeRedundants :: [Sub] -> [(SType, UIdent)] -> [(SType, UIdent)]
-- removeRedundants subs = filter

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

-- function for solving type equations
-- fold on TypeEqns is used to implement the algorithm
-- ON TypeEqn  ->
-- unify two terms with each other to obtain list of subs,
-- append all vars of the two terms together to obtain the free vars,
-- empty list as bound vars because there are no bound vars
-- ON TypeExist ->
-- gather all inner equations into the form of a solved equation ([v], [v], [sub])
-- call restricted linearize for bound vars to sub out the bound vars
-- and get remaining bound vars and subs
-- remove bound vars from free vars to get new free vars
-- pass up (new free vars, remaining bounds, remaining subs)
solveEquations :: TyRel -> TypeEqns -> Either String SolvedEqn
solveEquations tyrel eqns = foldTypeEqns (procExist tyrel) procEqn eqns

procExist ::
  TyRel ->
  [LIdent] ->
  [TyC] ->
  [Either String SolvedEqn] ->
  Either String ([LIdent], [LIdent], [(SType, UIdent)], [Sub])
procExist tyrel bounds constraints eqns = do
  let parsedConstraints = map (\(TypeConstraint u l) -> (l, u)) constraints
  (innerFrees, innerBounds, innerConstraints, innerSubs) <- getAllEqns eqns
  -- we should call restricted linearize for ALL bound vars
  -- so we append inner bounds and current bounds
  -- and then restricted linearize
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

-- function to get all variables of a lambda type term
vars :: SType -> [LIdent]
vars (TVar_SType a) = [a]
vars (Arrow_SType l r) = vars l ++ vars r
vars (List_SType a) = vars a
vars _ = []

-- function for applying substitutions
-- if first one is a type var then just check if the var that is going to be substituted is equal to that var, return the term
-- which is specified in sub otherwise return the type var itself
-- in other cases if there are inner terms, call the sub func recursively on them otherwise return themselves as there are no terms to apply substitution on
sub :: SType -> Sub -> SType
sub (TVar_SType z) (x, l)
  | x == z = l
  | otherwise = TVar_SType z
sub (Arrow_SType l r) s = Arrow_SType (sub l s) (sub r s)
sub (List_SType a) s = List_SType (sub a s)
sub Bool_SType _ = Bool_SType

-- function for detecting occurs check in a sub (x, t)
-- if x should be substituted with TypeVar x then it is trivial and we don't need to include it to list of subs
-- if x is exists in t, then it is occurs check because we can do substitution infinitely so it is occurs check failure
-- else add the sub to the list of subs because it is OK
check :: Sub -> Either String [Sub]
check (y, TVar_SType x) | x == y = return []
check sub@(x, term)
  | x `elem` vars term = Left "occurs check failure"
  | otherwise = return [sub]

-- function to match to type terms
-- when matching a term with a type variable we need to check for occurs check (check function above)
-- and otherwise just calling the match function on the inner terms
-- if the structure of two terms is different then it is a matching error (match _ _)
match :: (SType, SType) -> Either String [Sub]
match (TVar_SType x, t) = check (x, t)
match (t, TVar_SType x) = check (x, t)
match (Bool_SType, Bool_SType) = Right []
match (Arrow_SType l r, Arrow_SType l' r') = do
  s <- mapM match (zip [l, r] [l', r'])
  return (concat s)
match (List_SType t, List_SType t') = match (t, t')
match (x, y) = Left ("matching error " ++ show x ++ " " ++ show y)

-- coalesce function takes a substitution (x, t) and a list of substitutions
-- it checks each sub (y, s) in the list of subs
-- if x == y then  then t and s need to be matched to get new subs and then recursively
-- calls coalesce for the rest of subs and prepends new sub to the result
-- otherwise, we should apply substitution in s to get new sub, then check if it has occurs check with y
-- and recursively call coalesce for the rest subs
-- append new and rest subs together and return them as result
coalesce :: Sub -> [Sub] -> Either String [Sub]
coalesce _ [] = Right []
coalesce (x, t) ((y, s) : rest)
  | x == y = do
      matchSubs <- match (t, s)
      recSubs <- coalesce (x, t) rest
      return (matchSubs ++ recSubs)
  | otherwise = do
      let newS = sub s (x, t)
      newSub <- check (y, newS)
      recSubs <- coalesce (x, t) rest
      return (newSub ++ recSubs)

-- linearize takes a list of subs, calls coalesce for each sub and rest of the subs
-- to apply subs into other subs if exist
-- the result is a linear list of substitutions (non of them can be applied into another)
linearize :: [Sub] -> Either String [Sub]
linearize [] = return []
linearize (sub : rest) = do
  c <- coalesce sub rest
  l <- linearize c
  return (sub : l)

-- unification of two type terms
-- first match them to get list of possible subs
-- then linearize the subs
unify :: (SType, SType) -> Either String [Sub]
unify eq = do
  subs <- match eq
  linearize subs

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
newTypeVar :: ExprInferer LIdent
newTypeVar = do
  (counter, _) <- get
  setCounter (counter + 1)
  return (LIdent ("T" ++ show counter))

-- a function for running type judgement
-- runJudge :: Prog -> Either String SolvedEqn
-- runJudge (Dummy_Prog classDecs instanceDecs exps) = do
--   let Class_Dec name tyVar ops = head classDecs
--   let env = judgeClassDec ops 0 name tyVar
--   result <- evalStateT (judgeExpr (head exps) (LIdent "Q")) (0, [], env)
--   -- let all = TypeExist [tyVar, LIdent "T0"] [TypeConstraint name (TVar_SType tyVar)] (result : innerEqns)
--   (f, b, c, subs) <- solveEquations result
--   return (f, b, c, subs)

-- (finalConstraints, finalSubs) <- emptyFrees (filter (/= LIdent "Q") f) b c subs
-- return finalSubs
type CEnv = [(UIdent, [TyC], [(LIdent, SType)])]

judgeClassDec :: [ClassDec] -> CEnv
judgeClassDec = foldr f []
  where
    f (Class_Dec clsName tyVar ops) acc = (clsName, [TypeConstraint clsName (TVar_SType tyVar)], map (\(ClassOp_Dec opName opType) -> (opName, opType)) ops) : acc

data Dictionary = SimpDict [Expr] | OvDict (Dictionary -> [Expr])

type IEnv = [(String, Dictionary)]

type IEnvSel = [(LIdent, Dictionary -> Expr)]

sth :: [InstDec] -> (IEnv, [IEnvSel])
sth = foldr f ([], [])
  where
    f (Inst_Dec (UIdent u) s ops) (ienv, ienvsels) =
      ( ( "dict" ++ u ++ show s,
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

mshow (List_SType _) = "List"
mshow Bool_SType = "Bool"
mshow (Arrow_SType _ _) = "Fun"
mshow (TCons_SType (UIdent u) _) = u

judgeInstDec :: [InstDec] -> TyRel
judgeInstDec = foldr f ([], [])
  where
    f (Inst_Dec u s ops) (cacc, acc) = (cacc, (s, u) : acc)
    f (Inst_Dec_With_Constraint tyc u s ops) (cacc, acc) = ((s, tyc) : cacc, acc)

judgeExpr :: CEnv -> IEnv -> Expr -> LIdent -> ExprInferer (Either String SolvedEqn, Expr, [Dictionary])
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
      Nothing -> throwError' ("free" ++ show x)
      Just p -> return (procEqn (TVar_SType qType, TVar_SType p), Var_Expr x, [])
    [(UIdent u, tycs, ops)] -> case lookup x ops of
      Nothing -> throwError' "Error"
      Just s -> case lookup ("dict" ++ u ++ mshow s) ienv of
        Nothing -> throwError' ("no instance declaration for " ++ show x)
        Just dict ->
          return
            ( procExist ([], []) (vars s) tycs [procEqn (TVar_SType qType, s)],
              App_Expr (Var_Expr x) (Var_Expr (LIdent ("dict" ++ u ++ mshow s))),
              [dict]
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
--                     (True_Expr (Dummy.Abs.True "True"))
--                 )
--             )
--         ]
--     ]
--     [ Ass_Expr
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

insts :: [InstDec]
insts =
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