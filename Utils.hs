module Utils where
import Debug.Trace

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = if x `elem` xs then unique xs else x : unique xs

(!++!) :: Eq a => [a] -> [a] -> [a]
(!++!) = (++) . unique

mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (x, y) = (f x, y)

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

replace :: Eq a => a -> a -> [a] -> [a]
replace a1 a2 as = a2 : filter (/= a1) as