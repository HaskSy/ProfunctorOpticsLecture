module Optics where

import Basic
import Profunctor

type Optic p a b s t = p a b -> p s t

type IsoP a b s t = forall p. Profunctor p => Optic p a b s t
type LensP a b s t = forall p. Cartesian p => Optic p a b s t
type PrismP a b s t = forall p. Cocartesian p => Optic p a b s t
type TraversalP a b s t = forall p. Monoidal p => Optic p a b s t

isoC2P :: Iso a b s t -> IsoP a b s t
isoC2P (Iso to from) = dimap to from

isoP2C :: IsoP a b s t -> Iso a b s t
isoP2C l = l (Iso id id)
