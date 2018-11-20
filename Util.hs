module Util ( toText, invertMap ) where
import Data.Text.Lazy.Builder ( Builder, toLazyText ) -- text
import Data.Text.Lazy ( toStrict ) -- text
import qualified Data.Text as S ( Text ) -- text
import qualified Data.Map as M -- containers

toText :: Builder -> S.Text
toText = toStrict . toLazyText

invertMap :: (Ord v) => M.Map k v -> M.Map v k
invertMap = M.fromList . map (\(x,y) -> (y,x)) . M.assocs
