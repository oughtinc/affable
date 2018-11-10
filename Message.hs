module Message where
import Control.Applicative ( (<*>), pure, (*>) ) -- base
import Data.List ( mapAccumL ) -- base
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text
import Data.Void ( Void ) -- base
import Text.Megaparsec ( Parsec, many, some, takeWhile1P, (<|>), (<?>) ) -- megaparsec
import Text.Megaparsec.Char ( char ) -- megaparsec
import Text.Megaparsec.Char.Lexer ( decimal ) -- megaparsec

type Pointer = Int
type Address = Int

-- TODO: Perhaps add support for logic variables (?X perhaps) so this code can be shared.
data Message 
    = Text Text
    | Reference Pointer
    | Location Address
    | Structured [Message]
  deriving ( Eq, Ord, Show ) -- TODO: Implement custom Show.

{-
Pointer ::= "$" [1-9]*[0-9]

Address ::= "@" [1-9]*[0-9]

Msg ::= Pointer
      | Address
      | [^\]\[]+
      | "[" Msg* "]"
-}

pointerParser :: Parsec Void Text Pointer
pointerParser =  (char '$' *> decimal) <?> "pointer"

addressParser :: Parsec Void Text Address
addressParser =  (char '@' *> decimal) <?> "address"

messageParser :: Parsec Void Text Message
messageParser = Structured <$> some mParser <?> "message"
    where mParser = (Reference <$> pointerParser)
                <|> (Location <$> addressParser)
                <|> (Structured <$> (char '[' *> many mParser <* char ']') <?> "submessage")
                <|> (Text <$> takeWhile1P Nothing (\c -> c `notElem` "[]$@") <?> "text")

-- TODO: Change this.
type PointerEnvironment = M.Map Pointer Message

-- TODO: Use strict map.
type PointerRemapping = M.Map Pointer Pointer

expandPointers :: PointerEnvironment -> Message -> Message
expandPointers env       (Reference p) = case M.lookup p env of
                                            Nothing -> Reference p
                                            Just m -> expandPointers env m
expandPointers env (Structured blocks) = Structured (map (expandPointers env) blocks)
expandPointers env                   t = t

-- Given a Message, replace all Structured sub-Messages with pointers and output a mapping
-- from those pointers to the Structured sub-Messages.
normalizeMessage :: Int -> Message -> (PointerEnvironment, Message)
normalizeMessage start = go True M.empty
    where go isTopLevel env m@(Structured ms) 
            = let p = M.size env + start
                  result = if isTopLevel then Structured ms' else Reference p
                  env' = M.insert p (Structured ms') env
                  (env'', ms') = mapAccumL (go False) env' ms -- A bit of knot typing occurring here.
              in (env'', result)
          go _ env m = (env, m)

-- Assumes that the mapping is a bijection, i.e. every key maps to a distinct value.
invertMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invertMap = M.fromList . map (\(x,y) -> (y,x)) . M.assocs 

renumberMessage :: Int -> Message -> (PointerRemapping, Message)
renumberMessage start m = let (env, m') = go M.empty m in (invertMap env, m')
    where go env (Reference p) = case M.lookup p env of
                                    Just n -> (env, Reference n)
                                    Nothing -> let !n = M.size env + start 
                                               in (M.insert p n env, Reference n)
          go env (Structured ms) = let (env', ms') = mapAccumL go env ms
                                   in (env', Structured ms')
          go env m = (env, m)
