{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Time where
import Data.Aeson ( ToJSON, FromJSON ) -- aeson
import Data.Int ( Int64 ) -- base
import GHC.Generics ( Generic ) -- ghc

type LogicalTime = Int64

newtype Time = Time LogicalTime
    deriving ( Eq, Ord, Num, Show, Generic )

instance FromJSON Time
instance ToJSON Time
