{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Servant.API.Required where

import Data.Typeable (Typeable)

-- | Combinators can use this type to indicate whether the value is
--   required or not.
data Required
  = Required
  | NotRequired
  deriving (Show, Eq, Ord, Enum, Typeable)

