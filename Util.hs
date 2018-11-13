module Util ( toText ) where
import Data.Text.Lazy.Builder ( Builder, toLazyText ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import qualified Data.Text as S ( Text ) -- text

toText :: Builder -> S.Text
toText = toStrict . toLazyText
