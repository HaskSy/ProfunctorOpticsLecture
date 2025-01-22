{-# LANGUAGE GADTs #-}

module StateExample where
import Basic

data State s a where
  State :: {runState :: s -> (a, s)} -> State s a


inc :: Bool -> State Integer Bool
inc b = State {
    runState = \n -> (b, n + 1)
}

instance Functor (State s) where
    fmap f m = State $ \s ->
        let (x, s') = runState m s in (f x, s')


instance Applicative (State s) where
    pure x = State (x,)
    mf <*> mx = State $ \s ->
        let (f, s') = runState mf s
            (x, s'') = runState mx s'
        in (f x, s'')


data Tree a = Empty | Node (Tree a) a (Tree a)

inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder _ Empty = pure Empty
inorder m (Node t x u) = ((Node <$> inorder m t) <*> m x) <*> inorder m u

countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True

countOddInTree :: Tree Integer -> State Integer (Tree Bool)
countOddInTree = inorder countOdd


out :: FunList a b t -> Either t (a, FunList a b (b -> t))
out (Done t) = Left t
out (More x l) = Right (x, l)

inn :: Either t (a, FunList a b (b -> t)) -> FunList a b t
inn (Left t) = Done t
inn (Right (x, l)) = More x l

single :: a -> FunList a b b
single x = More x (Done id)

fuse :: FunList b b t -> t
fuse (Done t) = t
fuse (More x l) = fuse l x

inorderC :: Traversal a b (Tree a) (Tree b)  
inorderC = Traversal (inorder single)

