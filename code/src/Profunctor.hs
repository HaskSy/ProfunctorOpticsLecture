{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}

module Profunctor where

import Data.Void (Void)
import Basic

cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
cross f g (a, b) = (f a, g b)

plus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
plus f g = either (Left . f) (Right . g)

runit :: (a, ()) -> a
runit (a, ()) =  a

runit' :: a -> (a, ())
runit' a = (a, ())

lunit :: ((), a) -> a
lunit ((), a) =  a

lunit' :: a -> ((), a)
lunit' a = ((), a)

rzero :: Either a Void -> a
rzero x = case x of
    Left a -> a
    Right v -> \case {} v

rzero' :: a -> Either a Void
rzero' = Left

lzero :: Either Void a -> a
lzero x = case x of
    Left v -> \case {} v
    Right a -> a

lzero' :: a -> Either Void a
lzero' = Right

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

assoc' :: ((a, b), c) -> (a, (b, c))
assoc' ((a, b), c) = (a, (b, c))

coassoc :: Either a (Either b c) -> Either (Either a b) c
coassoc t = case t of
  Left a -> Left (Left a)
  Right (Left b) -> Left (Right b)
  Right (Right c) -> Right c

coassoc' :: Either (Either a b) c -> Either a (Either b c)
coassoc' t = case t of
  Left (Left a) -> Left a
  Left (Right b) -> Right (Left b)
  Right c -> Right (Right c)

rstrength :: Functor f => (f a, b) -> f (a, b)
rstrength (fx, y) = fmap (,y) fx

lstrength :: Functor f => (a, f b) -> f (a, b)
lstrength (x, fy) = fmap (x,) fy

pair :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
pair h k (x, y) = (,) <$> h x <*> k y

class Profunctor p where
    dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

-- dimap id id = id
-- dimap (f' . f) (g . g') = dimap f g . dimap f' g'

instance Profunctor (->) where
    dimap f g h = g . h . f

data UpStar f a b where
  UpStar :: { unUpStar :: a -> f b } -> UpStar f a b

instance Functor f => Profunctor (UpStar f) where
    dimap f g (UpStar h) = UpStar (fmap g . h . f)

class Profunctor p => Cartesian p where
    first  :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)

instance Cartesian (->) where
    first h = cross h id
    second = cross id

instance Functor f => Cartesian (UpStar f) where
    first  (UpStar unUpStar) = UpStar (rstrength . cross unUpStar id)
    second (UpStar unUpStar) = UpStar (lstrength . cross id unUpStar)

class Profunctor p => Cocartesian p where
    left :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)

instance Applicative f => Cocartesian (UpStar f) where
    left  (UpStar unUpStar) = UpStar (either (fmap Left . unUpStar) (pure . Right))
    right (UpStar unUpStar) = UpStar (either (pure . Left) (fmap Right . unUpStar))

class Profunctor p => Monoidal p where
    par :: p a b -> p c d -> p (a, c) (b, d)
    empty :: p id id

instance Monoidal (->) where
    par = cross
    empty = id

instance Applicative f => Monoidal (UpStar f) where
    empty = UpStar pure
    par h k = UpStar (pair (unUpStar h) (unUpStar k))

-- Profunctor instances for explicit encoding
instance Profunctor (Iso a b) where
    dimap f g (Iso to from) = Iso (to . f) (g . from)

