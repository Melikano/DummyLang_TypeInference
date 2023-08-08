module Unification where
import Dummy.Abs
import Types
-- function to get all variables of a lambda type term
vars :: SType -> [String]
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
sub Int_SType _ = Int_SType

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
match (Int_SType, Int_SType) = Right []
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
