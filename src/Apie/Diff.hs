module Apie.Diff
  ( Diff(..)
  , Diffable
  , genericParseDiff
  , genericToDiff
  , apply
  , diff
  )
where

import Apie.Diff.Types (Diff(..))
import Apie.Diff.GParseJSON (genericParseDiff)
import Apie.Diff.GToJSON (genericToDiff)
import Apie.Diff.GDiffable (Diffable, apply, diff)
