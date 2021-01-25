module GridMotion where

import Control.Monad.State
    --( MonadState(put, get), evalState, State )   

data Position = P Int Int deriving Show

applyFirst :: (Int -> Int) -> Position -> Position
applyFirst f (P x y) = P (f x) y

applySecond :: (Int -> Int) -> Position -> Position
applySecond f (P x y) = P x (f y)

distance :: Position -> Position -> Int
distance (P x y) (P x' y') = abs (x - x') + abs (y - y')

play :: String -> State Position Position
play []     = do
    get

play (x:xs) = do
    pos <- get
    case x of
         'n' -> put $ applySecond (+1) pos
         'w' -> put $ applyFirst (\x -> x - 1) pos
         's' -> put $ applySecond (\y -> y - 1) pos
         'e' -> put $ applyFirst (+1) pos
         _ -> put pos
    play xs

startState = P 0 0

playGM = do
    putStrLn "Enter final position, x:"
    xStr <- getLine 
    putStrLn "Enter final position, x:"
    xStr <- getLine 
    putStrLn "Enter directions, e.g., nnwwwwn"
    input <- getLine
    print $ evalState (play input) startState