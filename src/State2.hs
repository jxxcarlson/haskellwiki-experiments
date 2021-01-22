module State2 where

import Control.Monad.State

type Stack = [Int]

-- pop = State $ \(x:xs) -> (x, xs)
-- push x = State $ \x xs -> ((), (x:xs))


-- foo :: State Stack Int
-- foo  = do
--         push 2
--         push 3
--         push 8
--         pop >>= (\n -> push (2 * n ))
--         pop