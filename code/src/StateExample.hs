module StateExample where

data State s a = State { runState :: s -> (a, s) }


inc :: Bool -> State Integer Bool
inc b = State {
    runState = \n -> (b, n + 1)
}

instance Functor (State s) where
    fmap f m = 
    State {
        runState s = let (x, s') = run m s in (f x, s')
    }

instance Applicative (State s) where
    pure x = State {
        runState = \s -> (x, s)
    }
    m (<*>) n = State {
        runState = \s ->
            let ()
    }

