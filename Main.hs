module Main where
import Data.String ( fromString ) -- base
import Text.Megaparsec ( parseTest ) -- megaparsec

import Message ( messageParser )
import Command ( commandParser )

main :: IO ()
main = example

-- example = parseTest messageParser (fromString "foo [bar baz [quux]] $2 @4")
example = parseTest commandParser (fromString "send @81 foo [bar baz [quux]] $2 @4")
