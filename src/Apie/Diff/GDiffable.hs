{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Apie.Diff.GDiffable (Diffable, apply, diff) where

import RIO hiding (to)
import Data.Kind (Type)
import GHC.Generics
import Apie.Diff.Types (Diff(..))


class Diffable (t :: (Type -> Type) -> Type) where
  diff :: t Identity -> t Identity -> t Diff
  default diff :: ( Generic (t Identity)
                  , Generic (t Diff)
                  , GDiffable (Rep (t Identity)) (Rep (t Diff)))
               => t Identity -> t Identity -> t Diff
  diff a b = to $ genericDiff (from a) (from b)

  apply :: t Identity -> t Diff -> t Identity
  default apply :: ( Generic (t Identity)
                   , Generic (t Diff)
                   , GDiffable (Rep (t Identity)) (Rep (t Diff)))
                => t Identity -> t Diff -> t Identity
  apply a b = to $ genericApply (from a) (from b)

class GDiffable f g where
  genericDiff  :: f (t Identity) -> f (t Identity) -> g (t Diff)
  genericApply :: f (t Identity) -> g (t Diff) -> f (t Identity)

instance GDiffable U1 U1 where
  genericDiff  _ _ = U1
  genericApply _ _ = U1

instance GDiffable f g => GDiffable (D1 c f) (D1 c g) where
  genericDiff  (M1 a) (M1 b) = M1 (genericDiff a b)
  genericApply (M1 a) (M1 b) = M1 (genericApply a b)

instance GDiffable f g => GDiffable (C1 c f) (C1 c g) where
  genericDiff  (M1 a) (M1 b) = M1 (genericDiff a b)
  genericApply (M1 a) (M1 b) = M1 (genericApply a b)

instance GDiffable f g => GDiffable (S1 s f) (S1 s g) where
  genericDiff  (M1 a) (M1 b) = M1 (genericDiff a b)
  genericApply (M1 a) (M1 b) = M1 (genericApply a b)

instance (GDiffable f1 g1, GDiffable f2 g2) => GDiffable (f1 :*: f2) (g1 :*: g2) where
  genericDiff (a1 :*: a2) (b1 :*: b2) =
    genericDiff a1 b1 :*: genericDiff a2 b2
  genericApply (a1 :*: a2) (b1 :*: b2) =
    genericApply a1 b1 :*: genericApply a2 b2

instance Eq a => GDiffable (K1 i a) (K1 i (Diff a)) where
  genericDiff (K1 a) (K1 b)
    | a == b    = K1 Unchanged
    | otherwise = K1 (Changed b)

  genericApply (K1 x) (K1 Unchanged)   = K1 x
  genericApply (K1 _) (K1 (Changed x)) = K1 x

instance GDiffable (K1 i a) (K1 i a) where
  genericDiff  (K1 _) (K1 x) = K1 x
  genericApply (K1 _) (K1 x) = K1 x
