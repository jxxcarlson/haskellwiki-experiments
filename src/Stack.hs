module Stack where

type Stack = [Int]

pop_ :: Stack -> (Int, Stack)
pop_ (x:xs) = (x, xs)

push_ :: Int -> Stack -> ((), Stack)
push_ x xs = ((), (x:xs))


pop = ST pop_
push x = ST (push_ x)

-- > app foo []
-- (16,[3,2])
foo :: ST Stack Int
foo  = do
        push 2
        push 3
        push 8
        pop >>= (\n -> push (2 * n ))
        pop


newtype ST s a = ST { app :: s -> (a, s) }

state :: (s -> (a, s)) -> ST s a
state f = ST f

instance Functor (ST s) where
    fmap f s = ST $ \t -> let (a,s') = app s t
                          in ( f a, s')


instance Applicative (ST s) where
     pure a = ST (\s -> (a, s))
     stf <*> stx = ST $ (\s ->
         let (f,s')   = app stf s 
             (x, s'') = app stx s'
         in (f x, s''))

instance Monad (ST s) where
   return a = ST $ \s -> (a, s)
   m >>= k = ST $ \s -> let (a, s') = app m s
                        in app (k a) s'



-- instance Monad (ST s) where  
--     return x = ST $ \s -> (x,s)  
--     (ST h) >>= f = ST $ \s -> let (a, newState) = h s  
--                                   (ST g) = f a  
--                               in  g newState 
                              
--   pure = return
--   (<*>) = ap