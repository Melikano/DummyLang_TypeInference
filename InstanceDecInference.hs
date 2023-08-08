module InstanceDecInference where

import Dummy.Abs
import Types
import Unification
import Utils

judgeInstDec :: [InstDec] -> TyRel
judgeInstDec = foldr f []
  where
    f (Inst_Dec_With_Constraint tycs u s ops) acc = case lookup u acc of
      Nothing -> (u, [(s, g tycs s)]) : acc
      Just entry -> (u, (s, g tycs s) : entry) : filter (\(u', _) -> u' /= u) acc
    f _ acc = acc
    g :: [TyC] -> SType -> SType -> Either String [TyC]
    g tycs s s' = do
      subs <- match (s, s')
      return $ map (\(TypeConstraint n st) -> TypeConstraint n (h st subs)) tycs
    h = foldr (flip sub)