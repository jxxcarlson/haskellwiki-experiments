module Lib where

import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

printMany :: (Show a) => [a] -> IO ()
printMany = mapM_ (putStrLn . show)