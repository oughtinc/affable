{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Time where

newtype Time = Time Int
    deriving ( Eq, Ord, Num, Show )


