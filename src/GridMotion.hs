module GridMotion where

import Control.Monad.State
    ( MonadState(put, get), evalState, State )   

data Position = P Int Int

applyFirst :: (Int -> Int) -> Position -> Position
applyFirst f (P x y) = P (f x) y

applySecond :: (Int -> Int) -> Position -> Position
applySecond f (P x y) = P x (f y)

-- play :: String -> State Bool Position
-- play []     = do
--     pos <- get
--     return pos

-- play (x:xs) = do
--     pos <- get
--     let (p:ps) = positions
--     case x of
--          'n' -> put (P { y = (y p) + 1 } p) positions
--          'w' -> put P { x = (x pos) + 1 }
--          's' -> put P { y = (y pos) + 1 }
--          'e' -> put P { x = (x pos) - 1 }
--          _        -> P $ put (on, score)
--     play xs

-- startState = (0, 0)

-- main = do
--     input <- getLine
--     print $ evalState (play input) startStatet