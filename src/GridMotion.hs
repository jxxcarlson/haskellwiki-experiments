module GridMotion where

import Control.Monad.State
    --( MonadState(put, get), evalState, State )   

data Position = P Int Int deriving Show

applyX :: (Int -> Int) -> Position -> Position
applyX f (P x y) = P (f x) y

applyY :: (Int -> Int) -> Position -> Position
applyY f (P x y) = P x (f y)

applyXGS :: (Int -> Int) -> GameState -> GameState
applyXGS f gs = gs { current = applyX f (current gs)}

applyYGS :: (Int -> Int) -> GameState -> GameState
applyYGS f gs = gs { current = applyY f (current gs)}

distance :: Position -> Position -> Int
distance (P x y) (P x' y') = abs (x - x') + abs (y - y')

data GameState  = GameState {current :: Position, goal :: Position}

play :: String -> State GameState Position
play []     = do
    gs <- get
    return (current gs)

play (x:xs) = do
    gs <- get
    let gs' = case x of
         'n' -> applyYGS (+1) gs
         'w' -> applyXGS (\x -> x - 1) gs
         's' -> applyYGS (\y -> y - 1) gs
         'e' -> applyXGS (+1) gs
         _ -> gs 
    put gs'    
    play xs

startState x y = GameState {current = P 0 0, goal = P x y}

playGM = do
    putStrLn "Enter final position, x:"
    xStr<- getLine 
    putStrLn "Enter final position, x:"
    yStr <- getLine 
    putStrLn "Enter directions, e.g., nnwwwwn"
    input <- getLine
    print $ evalState (play input) (startState (read xStr :: Int)  (read yStr :: Int))
    -- print $ evalState (play input) (startState 5  7)