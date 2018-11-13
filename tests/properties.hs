{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Foldable ( any ) -- base
import Data.String ( fromString ) -- base
import Data.Text ( Text, empty, cons ) -- text
import Test.QuickCheck -- QuickCheck
import Test.Framework.TH -- test-framework-th
import Test.Framework.Providers.QuickCheck2 -- test-framework
import Test.Feat ( deriveEnumerable, uniform, Enumerable(..), c2, datatype ) -- testing-feat

import Message

deriveEnumerable ''Message

-- This is probably terribly slow...
instance Enumerable Text where
    enumerate = datatype [pure empty, c2 cons]

instance Arbitrary Message where
    arbitrary = sized uniform

hasStructured :: Message -> Bool
hasStructured (Structured ms) = any isStructured ms
    where isStructured (Structured _) = True
          isStructured _ = False
hasStructured _ = False

prop_normalizeMessage_indirects_all_pointers msg = not (hasStructured $ snd $ normalizeMessage 0 msg)

main :: IO ()
main = $defaultMainGenerator
