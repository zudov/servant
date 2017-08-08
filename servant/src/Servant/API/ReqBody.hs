{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.ReqBody where

import           Data.Typeable (Typeable)

import           Servant.API.Required (Required(..))
-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >>>            -- POST /books
-- >>> type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
type ReqBody a = ReqBody' 'Required a

-- | Extract the request body (if it's provided) as a value of type @Maybe a@.
--
-- Example:
--
-- >>>            -- POST /books
-- >>> type MyApi = "books" :> OptionalReqBody '[JSON] Book :> Post '[JSON] Book
type OptionalReqBody a = ReqBody' 'NotRequired a

data ReqBody' (required :: Required) (contentTypes :: [*]) a
    deriving (Typeable)
-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
