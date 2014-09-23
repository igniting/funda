{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Funda.Backend.RangedBackend where

import           Funda.Backend.Backend

class (Backend b, Ord (Key b)) => RangedBackend b where
  type RangeM b :: * -> *
  getRange :: Key b -> Key b -> RangeM b [Value b]
