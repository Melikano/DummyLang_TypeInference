module TypeInference where

import Dummy.Abs
import Lib.Monads

-- | autoGraderTypeInference
-- You should implement this function! This function should take a LambdaTerm
-- and output either:
--      - Left: an informative error message (either a free variable error,
--          or a type inferencing error such as match failure or occurs check)
--      - Right: the most general inferred type of the lambda term
-- Wahoo!! Last assignment you're nearly there :D
throwError' :: String -> StateT s (Either String) a
throwError' error = lift (Left error)

type Judger a = StateT (Int, Context) (Either String) a

type Context = [(LIdent, LIdent)]

type Sub = (LIdent, SType)

-- data type for type equations:
-- they can either be type existence with list of bound type vars and list of type equations or type equality between two type terms
data TypeEqns = TypeExist [LIdent] [TypeEqns] | TypeEqn (SType, SType)
  deriving (Show)

type SolvedEqn = ([LIdent], [LIdent], [Sub])

-- fold function for typeEqn type
-- for TypeEqn constructor -> just apply g type equality
-- for TypeExist constructor -> apply f on list of bound variables and map fold on inner list of type eqns
foldTypeEqns ::
  ([LIdent] -> [b] -> b) -> ((SType, SType) -> b) -> TypeEqns -> b
foldTypeEqns f g (TypeEqn eq) = g eq
foldTypeEqns f g (TypeExist l eqs) = f l (map (foldTypeEqns f g) eqs)

-- linearize function restricted to input list of vars
-- takes each bound variable, finds the substitution for it (if exist)
-- applies the substitution and does the matching if needed (coalesce function)
-- recursively calls for the rest of bound variables
-- if there is no substitutions for a bound variable then it returns it as the list of not-substituted vars
-- so they can be passed up in the main algorithm
restrictedLinearize ::
  [LIdent] -> [LIdent] -> [Sub] -> Either String ([LIdent], [Sub])
restrictedLinearize [] rbs subs = return (rbs, subs)
restrictedLinearize (b : bs) rbs subs = do
  case lookup b subs of
    Nothing -> restrictedLinearize bs (b : rbs) subs
    Just s -> do
      cSubs <- coalesce (b, s) subs
      restrictedLinearize bs rbs cSubs

-- function that gathers all free variables, bound variables and substitutions of a list of equations into a
-- triple of form ([frees], [bounds], [subs]) of type SolvedEqns
getAllEqns ::
  [Either String SolvedEqn] -> Either String SolvedEqn
getAllEqns = foldr allEqns (Right ([], [], []))
  where
    allEqns newEitherFBS accEitherFBS = do
      (f, b, s) <- newEitherFBS
      (fs, bs, ss) <- accEitherFBS
      -- frees = union of all frees
      -- bounds = disjoint union of bounds (as they must be distinct)
      -- subs = first subs appended onto the second
      return (fs ++ f, b ++ bs, ss ++ s)


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
solveEquations :: TypeEqns -> Either String SolvedEqn
solveEquations = foldTypeEqns procExist procEqn
  where
    procExist ::
      [LIdent] ->
      [Either String ([LIdent], [LIdent], [Sub])] ->
      Either String ([LIdent], [LIdent], [Sub])

    procExist bounds eqns = do
      (innerFrees, innerBounds, innerSubs) <- getAllEqns eqns
      -- we should call restricted linearize for ALL bound vars
      -- so we append inner bounds and current bounds
      -- and then restricted linearize
      let allBounds = bounds ++ innerBounds
      (remainingBounds, remainingSubs) <-
        restrictedLinearize
          allBounds
          []
          innerSubs
      return
        ( filter (`notElem` allBounds) innerFrees,
          remainingBounds,
          remainingSubs
        )

    procEqn (lt1, lt2) = do
      let esubs = unify (lt1, lt2)
      subs <- esubs
      return (vars lt1 ++ vars lt2, [], subs)

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
  | otherwise = TVar_SType  z
sub (Arrow_SType l r) s = Arrow_SType  (sub l s) (sub r s)
sub (List_SType a) s = List_SType  (sub a s)

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
match (t, TVar_SType x) = check (x, t)
match (TVar_SType x, t) = check (x, t)
match (Arrow_SType l r, Arrow_SType l' r') = do
  s <- mapM match (zip [l, r] [l', r'])
  return (concat s)
match (List_SType t, List_SType t') = match (t, t')
match _ = Left "matching error"

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
autoGraderTypeInference :: Expr -> Either String SType
autoGraderTypeInference l = do
  result <- evalStateT (judge l (LIdent "q")) (0, [])
  (f, b, subs) <- solveEquations result
  finalSubs <- emptyFrees (filter (/= LIdent "q") f) subs
  case lookup "Q" (map (\(LIdent x, s) -> (x, s)) finalSubs) of
    Nothing -> Right (TVar_SType (LIdent "T0"))
    Just s -> Right s

-- call restricted linearize on free vars to get rid of them
emptyFrees :: [LIdent] -> [Sub] -> Either String [Sub]
emptyFrees [] subs = return subs
emptyFrees frees subs = do
  (f, ss) <- restrictedLinearize frees [] subs
  return ss

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

-- function that sets counter in a state (Ryan's Tutorial notes)
setCounter :: Int -> Judger ()
setCounter counter = do
  modify (mapFst (const counter))

-- function that sets context in a state
setContext :: Context -> Judger ()
setContext context = do
  modify (mapSnd (const context))

-- function that generates a new type var using the counter (Ryan's Tutorial notes)
newTypeVar :: Judger LIdent
newTypeVar = do
  (counter, _) <- get
  setCounter (counter + 1)
  return (LIdent ("T" ++ show counter))

-- a function for running type judgement
runJudge :: Expr -> Either String TypeEqns
runJudge l = evalStateT (judge l (LIdent "Q")) (0, [])

-- judge based on the judgments specified in the fixtypes.pdf
-- I really just translated them into haskell :)) I didn't do any important thing
judge :: Expr -> LIdent -> Judger TypeEqns
-- if var, look up for its type in the context, if not found it means that no it is free
-- otherwise the type var should be equal to the type found in context
judge (Var_Expr x) qType = do
  (_, context) <- get
  case lookup x context of
    Nothing -> throwError' "free"
    Just p -> return (TypeEqn (TVar_SType p, TVar_SType qType))
judge (Abst_Expr x t) qType = do
  (_, context) <- get
  xType <- newTypeVar
  tType <- newTypeVar
  setContext $ (x, xType) : context
  tEqns <- judge t tType
  setContext context
  let newEqn =
        TypeEqn
          (TVar_SType qType, Arrow_SType (TVar_SType xType) (TVar_SType tType))
  return (TypeExist [xType, tType] (newEqn : [tEqns]))
judge (App_Expr left right) qType = do
  lType <- newTypeVar
  rType <- newTypeVar
  lEqns <- judge left lType
  rEqns <- judge right rType
  let newEqn =
        TypeEqn
          (TVar_SType lType, Arrow_SType (TVar_SType  rType) (TVar_SType  qType))
  return (TypeExist [rType, lType] (newEqn : lEqns : [rEqns]))

judge (List_Expr Nil) qType = do
  aType <- newTypeVar
  return
    ( TypeExist
        [aType]
        [TypeEqn (TVar_SType  qType, List_SType  (TVar_SType  aType))]
    )
-- judge (List_Expr (Cons a as)) qType = do
--   aType <- newTypeVar
--   return
--     ( TypeExist
--         [aType]
--         [ TypeEqn
--             ( TVar_SType  qType,
--               Arrow_SType

--                 ( TyProduct

--                     (TVar_SType  aType)
--                     (TyList  (TVar_SType  aType))
--                 )
--                 (TyList  (TVar_SType  aType))
--             )
--         ]
--     )
-- judge (LCase_Expr t Nil t0 (Cons a as) t1) qType = do
--   (counter, context) <- get
--   tType <- newTypeVar
--   t0Type <- newTypeVar
--   vType <- newTypeVar
--   t1Type <- newTypeVar
--   xType <- newTypeVar
--   tEqns <- judge t tType
--   t0Eqns <- judge t0 t0Type
--   setContext ((v, vType) : context)
--   t1Eqns <- judge t1 t1Type
--   setContext context
--   let newEqns =
--         [ TypeEqn (TVar_SType  tType, List_SType  (TVar_SType  xType)),
--           TypeEqn (TVar_SType  t0Type, TVar_SType  qType),
--           TypeEqn
--             ( TVar_SType  vType,
--               TyProduct  (TVar_SType  xType) (TyList  (TVar_SType  xType))
--             ),
--           TypeEqn (TVar_SType  t1Type, TVar_SType  qType)
--         ]
--   return
--     ( TypeExist
--         [xType, tType, vType, t0Type, t1Type]
--         (newEqns ++ [tEqns] ++ [t0Eqns] ++ [t1Eqns])
--     )