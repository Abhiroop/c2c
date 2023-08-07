module Utils where

type Stack a = [a]

-- | push - let st' = x >: st
(>:) :: a -> Stack a -> Stack a
(>:) x xs = x:xs

-- | pop - let (x,xs) = <: st
(<:) :: Stack a -> (a, Stack a)
(<:) (x:xs) = (x, xs)

-- | init stack
initStack :: Stack a
initStack = []
