module Utils where

import Data.Map.Strict

type Stack a = [a]

-- | push - let st' = x >: st
(>:) :: a -> Stack a -> Stack a
(>:) x xs = x:xs

-- | push many - let st' = xs >>: st
(>>:) :: [a] -> Stack a -> Stack a
(>>:) xs ys = xs ++ ys


-- | pop - let (x,xs) = <: st
(<:) :: Stack a -> (a, Stack a)
(<:) (x:xs) = (x, xs)

-- | init stack
initStack :: Stack a
initStack = []

insertMany :: (Ord k) => [(k, v)] -> Map k v -> Map k v
insertMany entries m = fromList $ toList m ++ entries
