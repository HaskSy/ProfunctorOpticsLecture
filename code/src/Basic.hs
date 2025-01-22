module Basic where
import GHC.Base (build)

-- Lens
data Lens a b s t = Lens { view' :: s -> a, update' :: (b, s) -> t }

type LensM a s = Lens a a s s

-- Prism
data Prism a b s t = Prism { preview' :: s -> Either t a, build' :: b -> t }

type PrismM a s = Prism a a s s

-- Iso
data Iso a b s t = Iso { to' :: s -> a, from' :: b -> t }

type IsoM a s = Iso a a s s

-- Traversal
data FunList a b t = Done t | More a (FunList a b (b -> t))

newtype Traversal a b s t = Traversal { traverse' :: s -> FunList a b t }

instance Functor (FunList a b) where
    fmap f (Done t) = Done (f t)
    fmap f (More x l) = More x (fmap (f .) l)

instance Applicative (FunList a b) where
    pure = Done
    Done f <*> l' = fmap f l'
    More x l <*> l' = More x (fmap flip l <*> l')


-- Lens Examples
p1 :: Lens a b (a, c) (b, c)
p1 = Lens { view' = fst, update' = \(x', (_ ,y)) -> (x', y) }

sign :: Lens Bool Bool Integer Integer 
sign = Lens {
    view' = (>= 0),
    update' = \(b, x) -> if b then abs x else -(abs x)
}

p11 :: Lens a b ((a, c), d) ((b, c), d)
p11 =
    let v = view' p1 in
    let u = update' p1 in
    Lens {
        view' = v . v,
        update' = \(f, a) -> u (u (f, v a), a)
    }

composeLenses :: Lens c d a b -> Lens e f c d -> Lens e f a b
composeLenses (Lens view1 update1) (Lens view2 update2) = Lens
    { view' = view2 . view1
    , update' = \ (f, a) -> update1 (update2 (f, view1 a), a)
    }

-- ~~ Lens Examples ~~

-- Prism Examples
the :: Prism a b (Maybe a) (Maybe b)
the = Prism {
    preview' = \case {
        Just x -> Right x;
        Nothing -> Left Nothing
    },
    build' = Just
}

composePrism :: Prism u v s t  -> Prism a b u v  -> Prism a b s t  
composePrism (Prism preview1 build1) (Prism preview2 build2) = Prism {
    preview' = \s -> case preview1 s of
        Left t -> Left t
        Right u -> case preview2 u of 
            Left v -> Left $ build1 v
            Right a -> Right a,
    build' = build1 . build2
}

-- ~~ Prism Exampes ~~

-- Iso Examples

flatten :: IsoM (a, b, c) ((a, b), c)
flatten = Iso {
    to' = \((a, b), c) -> (a, b, c),
    from' = \(a, b, c) -> ((a, b), c)
}

composeIso :: Iso u v s t -> Iso a b u v -> Iso a b s t
composeIso (Iso to1 from1) (Iso to2 from2) = Iso to from
    where 
        from = from1 . from2
        to = to2 . to1 

-- ~~ Iso Examples

-- Traversal Examples

-- composeTraversal :: Traversal s t u v -> Traversal u v a b -> Traversal a b s t
-- composeTraversal (Traversal traverse1) (Traversal traverse2) = Traversal $ \s ->
--     case traverse1 s of
--         Done t -> Done t
--         More u rest -> case traverse2 u of
--             Done v -> fmap (\cont -> rest (const v)) (Done v)
--             More a restInner -> More a (fmap (\cont -> More (cont a) rest) restInner)
--

-- ~~ Traversal Examples ~~

