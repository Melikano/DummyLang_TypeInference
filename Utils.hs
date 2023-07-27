module Utils where
import Types
import Lib.Monads
import Dummy.Abs

throwError' :: String -> StateT s (Either String) a
throwError' error = lift (Left error)

foldTypeEqns ::
  ([LIdent] -> [TyC] -> [b] -> b) -> ((SType, SType) -> b) -> TypeEqns -> b
foldTypeEqns f g (TypeEqn eq) = g eq
foldTypeEqns f g (TypeExist l cs eqs) = f l cs (map (foldTypeEqns f g) eqs)

unique [] = []
unique (x : xs) = if x `elem` xs then unique xs else x : (unique xs)

vars :: SType -> [LIdent]
vars (TVar_SType a) = [a]
vars (Arrow_SType l r) = vars l ++ vars r
vars (List_SType a) = vars a
vars _ = []


mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (x, y) = (f x, y)

replace :: (SType, UIdent) -> (SType, UIdent) -> [(SType, UIdent)] -> [(SType, UIdent)]
replace a1 a2 as = a2 : filter (/= a1) as


mshow (List_SType _) = "List"
mshow Bool_SType = "Bool"
mshow (Arrow_SType _ _) = "Fun"
mshow (TCons_SType (UIdent u) _) = u
