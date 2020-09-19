{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Higher-Kinded Data
--
-- See:
-- - https://reasonablypolymorphic.com/blog/higher-kinded-data/

module Apie.HKD
  ( HKD
  )
where

import Data.Functor.Identity (Identity)

type family HKD f a where
    HKD Identity a = a
    HKD f a = f a
