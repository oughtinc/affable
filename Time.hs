{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Time where
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import GHC.Generics ( Generic ) -- ghc

newtype Time = Time Int
    deriving ( Eq, Ord, Num, Show, Generic )

instance FromJSON Time
instance ToJSON Time
