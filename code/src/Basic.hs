module Basic where

-- Iso
data Iso s t a b = Iso { to' :: s -> a, from' :: b -> t }

type IsoM s a = Iso s s a a

-- Lens
data Lens s t a b = Lens { view' :: s -> a, update' :: (b, s) -> t }

type LensM s a = Lens s s a a

-- Prism
data Prism s t a b = Prism { preview' :: s -> Either t a, build' :: b -> t }

type PrismM s a = Prism s s a a

-- Traversal
data FunList a b t = Done t | More a (FunList a b (b -> t))

newtype Traversal s t a b = Traversal { traverse' :: s -> FunList a b t }

-- Utils
p1 :: Lens (a, c) (b, c) a b
p1 = Lens { view' = fst, update' = \(x', (_ ,y)) -> (x', y) }


sign :: Lens Integer Integer Bool Bool
sign = Lens { 
    view' = (>= 0), 
    update' = \(b, x) -> if b then abs x else -(abs x)
}

the :: Prism (Maybe a) (Maybe b) a b
the = Prism {
    preview' = \case {
        Just x -> Right x;
        Nothing -> Left Nothing
    },
    build' = Just
}

-- whole :: Prism Integer Integer Double Double

-- p11

flatten :: IsoM ((a, b), c) (a, b, c)
flatten = Iso {
    to' = \((a, b), c) -> (a, b, c),
    from' = \(a, b, c) -> ((a, b), c)
}
