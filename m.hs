module M where
    class Melika a where
        f :: a -> a -> a
        
    instance (Melika a) => Melika [a] where
        f :: Melika a => [a] -> [a] -> [a]
        f = \xs -> \ys -> f (head xs) (head ys) : f (tail xs) (tail ys)
