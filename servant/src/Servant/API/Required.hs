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

-- | This type is a lot like 'Maybe', but has a phantom 'Required' parameter.
--
--   The constructors capture the idea that if the value is 'Provided' it can be
--   either 'Required' or 'NotRequired', but if the value is 'NotProvided' it has
--   to be 'NotRequired'.
data Provided (r :: Required) a where
  Provided    :: a -> Provided r a
  NotProvided :: Provided 'NotRequired a

deriving instance Show a => Show (Provided r a)
deriving instance Eq   a => Eq   (Provided r a)
deriving instance Ord  a => Ord  (Provided r a)

deriving instance Typeable Provided
deriving instance Typeable (Provided r)
deriving instance Typeable (Provided r a)

-- | If the type of 'Provided' value is 'Required' it can be unwrapped without
--   handling the 'NotProvided' case.
runProvided :: Provided 'Required a -> a
runProvided (Provided a) = a

-- | By providing a value for 'NotProvided' case, any 'Provided' value can be
--   coerced to be 'Required'.
requireProvided :: a -> Provided r a -> Provided 'Required a
requireProvided a NotProvided = Provided a
requireProvided _ (Provided p) = Provided p
