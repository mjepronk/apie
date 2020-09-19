module Apie.Diff.Types where

-- This module provides a Diff (or delta) type that is useful when we want to
-- implement simple CRUD operations in event sourcing. The way I've chosen to
-- model this is by using a Higher Kinded Datatype (HKD) that can take a `Diff`
-- type as an argument. Passing `Identity` to the HKD will give us the type for
-- the basic record.
--
-- The `Diff` type is isomorphic to Maybe: `Changed a` for fields for which the
-- value has been changed or `Unchanged` if the field hasn't changed in a
-- particular event.
--
-- See:
-- https://reasonablypolymorphic.com/blog/higher-kinded-data/
-- https://github.com/trevorcook/hkd-delta
--
-- There is a lot of GHC Generics here to scrap your boiler-plate code.
--
-- See:
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html


import RIO

data Diff a
  = Changed !a
  | Unchanged

instance Show a => Show (Diff a) where
  show (Changed x) = "(Changed " <> show x <> ")"
  show Unchanged = "Unchanged"

instance Eq a => Eq (Diff a) where
  Changed x == Changed y = x == y
  Unchanged == Unchanged = True
  _ == _ = False
