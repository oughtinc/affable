{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Primitive ( makePrimitive, makePrimitives, primitives ) where
import Control.Applicative ( liftA2 ) -- base
import Data.Foldable ( asum ) -- base
import Data.Functor ( (<$) ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Text.Read ( signed, decimal ) -- text

import Exp ( Primitive, PrimEnv, Pattern, Value )
import Message ( Message(..), matchMessage )
import Scheduler ( SchedulerContext(..), relabelMessage, fullyExpand )
import Workspace ( WorkspaceId )

don'tKnow :: Message
don'tKnow = Structured [Text "Don't know"]

-- TODO: Assumes variables in pattern are in ascending order.
makePrimitive :: SchedulerContext extra -> Pattern -> ([Value] -> IO Value) -> (WorkspaceId -> Value -> IO Value)
makePrimitive ctxt pattern body workspaceId msg = do
    expanded <- fullyExpand ctxt msg
    answer <- maybe (return don'tKnow) (body . M.elems) . matchMessage pattern $ expanded
    workspace <- getWorkspace ctxt workspaceId
    newWorkspaceId <- createWorkspace ctxt False workspace msg pattern
    workspace <- getWorkspace ctxt newWorkspaceId
    answer <- normalize ctxt answer
    answer <- relabelMessage ctxt answer
    sendAnswer ctxt False workspace answer
    return answer

makePrimitives :: SchedulerContext extra -> (WorkspaceId -> Value -> IO ()) -> IO (PrimEnv WorkspaceId IO Primitive, Value -> Maybe Primitive)
makePrimitives ctxt recordAnswer = return (primEnv, matchPrim)
    where !primEnv = M.fromList $ map (\(p, pattern, _, body) -> (p, postProcess (makePrimitive ctxt pattern body))) primitives
          -- TODO: This could be more efficient.
          matchPrim msg = asum $ map (\(p, pattern, _, _) -> p <$ matchMessage pattern msg) primitives -- TODO: Ensure only one match.
          postProcess prim wsId v = do
            answer <- prim wsId v
            recordAnswer wsId answer
            return answer

-- TODO: Include a way to output Haskell code.
primitives :: [(Primitive, Pattern, T.Text, [Value] -> IO Value)]
primitives = [
    (1, Structured [Text "add ", Reference 0, Text " ", Reference 1], "case (p0, p1) of (Structured [Text x],Structured [Text y]) -> Structured [Text \"result \", Structured [Text (show (read x + read y))]]; _ -> Structured [Text \"Don't know\"]", addPrim)
  ]

textAsInt :: T.Text -> Maybe Int
textAsInt t = case signed decimal t of
                Left _ -> Nothing
                Right (n, r) | T.null r -> Just n
                             | otherwise -> Nothing -- There was extra garbage.

asResult :: (Show a) => a -> Message
asResult x = Structured [Text "result ", Structured [Text (fromString (show x))]]

addPrim :: [Value] -> IO Value
addPrim [Structured [Text x], Structured [Text y]] = return $ maybe don'tKnow asResult $ liftA2 (+) (textAsInt x) (textAsInt y)
addPrim _ = return don'tKnow
