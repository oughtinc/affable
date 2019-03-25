{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Foldable ( any ) -- base
import Data.List ( group, sort ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import Data.Text ( Text, empty, cons ) -- text
import Data.UUID ( UUID, fromWords ) -- uuid
import Test.QuickCheck -- QuickCheck
import Test.Framework.TH -- test-framework-th
import Test.Framework.Providers.QuickCheck2 -- test-framework
import Test.Feat ( deriveEnumerable, uniform, Enumerable(..), c2, c4, datatype ) -- testing-feat

import Message

deriveEnumerable ''Message

-- This is probably terribly slow...
instance Enumerable Text where
    enumerate = datatype [pure empty, c2 cons]

instance Enumerable UUID where
    enumerate = datatype [c4 fromWords]

instance Arbitrary Message where
    arbitrary = sized uniform

hasStructured :: Message -> Bool
hasStructured (Structured ms) = any isStructured ms
    where isStructured (Structured _) = True
          isStructured _ = False
hasStructured _ = False

prop_normalizeMessage_indirects_all_pointers msg = not (hasStructured $ snd $ normalizeMessage 0 msg)

prop_matchMessage_unit_1
    = matchMessage (Structured [Reference 1, Reference 2]) (Structured [Structured [Text "foo"], Reference 10]) == Just (M.fromList [(1,Structured [Text "foo"]),(2,Reference 10)])

prop_matchMessage_unit_2
    = matchMessage (Structured [Reference 1, Reference 2]) (Structured [Text "foo", Reference 10]) == Nothing

prop_collectPointers_unit_1 = collectPointers (Structured [Structured [Reference 1, Reference 2], Reference 3]) == [1,2,3]

allDistinct :: (Ord a) => [a] -> Bool
allDistinct = all ((1==) . length) . group . sort

prop_generalizeMessage_unit_1 = allDistinct $ collectPointers (snd (generalizeMessage 1000 m))
    where m = Structured [Reference 1, Reference 1]

prop_generalizeMessage_all_pointers_unique m = allDistinct $ collectPointers (snd (generalizeMessage 1000 m))

main :: IO ()
main = $defaultMainGenerator
