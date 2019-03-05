{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Primitive ( PrimFunc, makePrimitive, makePrimitives, primitives ) where
import Control.Applicative ( liftA2 ) -- base
import Data.Foldable ( asum ) -- base
import Data.Functor ( (<$) ) -- base
import qualified Data.Map as M -- containers
import Data.String ( fromString ) -- base
import qualified Data.Text as T -- text
import Data.Text.Read ( signed, decimal ) -- text

import Exp ( Primitive, PrimEnv, Pattern, Value )
import Message ( Message(..), matchMessage )
import Scheduler ( SchedulerContext(..), autoUserId, relabelMessage, fullyExpand, normalize )
import Workspace ( WorkspaceId, parentId )

type PrimFunc extra = SchedulerContext extra -> WorkspaceId -> [Value] -> IO Value

don'tKnow :: Message
don'tKnow = Structured [Text "Don't know"]

-- TODO: Assumes variables in pattern are in ascending order.
makePrimitive :: SchedulerContext extra -> Pattern -> PrimFunc extra -> (WorkspaceId -> Value -> IO Value)
makePrimitive ctxt pattern body workspaceId msg = do
    answer <- maybe (return don'tKnow) (body ctxt workspaceId . M.elems) . matchMessage pattern =<< fullyExpand ctxt msg
    answer <- relabelMessage ctxt =<< normalize ctxt answer
    sendAnswer ctxt False autoUserId workspaceId answer
    return answer

makePrimitives :: SchedulerContext extra -> IO (PrimEnv WorkspaceId IO Primitive, Value -> Maybe Primitive)
makePrimitives ctxt = return (primEnv, matchPrim)
    where !primEnv = M.fromList $ map (\(p, pattern, _, body) -> (p, makePrimitive ctxt pattern body)) primitives
          -- TODO: This could be more efficient.
          matchPrim msg = asum $ map (\(p, pattern, _, _) -> p <$ matchMessage pattern msg) primitives -- TODO: Ensure only one match.

-- TODO: Make it so the Haskell code definitions are written into a Primitives module (similar to the gen-api commandline option)
-- and simply have the generated code reference this module.
primitives :: [(Primitive, Pattern, T.Text, PrimFunc extra)]
primitives = [
    (1, Structured [Text "add ", Reference 0, Text " ", Reference 1], binPrimCode "(+)", binIntPrim (+)),
    (2, Structured [Text "reflect"], "error \"reflect doesn't work in exported code currently \"", reflectPrim)
  ]

textAsInt :: T.Text -> Maybe Int
textAsInt t = case signed decimal t of
                Left _ -> Nothing
                Right (n, r) | T.null r -> Just n
                             | otherwise -> Nothing -- There was extra garbage.

asResult :: (Show a) => a -> Message
asResult x = Structured [Text "result ", Structured [Text (fromString (show x))]]

binIntPrim :: (Show a) => (Int -> Int -> a) -> PrimFunc extra
binIntPrim f _ _ [Structured [Text x], Structured [Text y]] = return $ maybe don'tKnow asResult $ liftA2 f (textAsInt x) (textAsInt y)
binIntPrim _ _ _ _ = return don'tKnow

reflectPrim :: PrimFunc extra
reflectPrim ctxt workspaceId _ = do
    workspace <- getWorkspace ctxt workspaceId
    maybe (return don'tKnow) (reifyWorkspace ctxt) (parentId workspace)

binPrimCode :: T.Text -> T.Text
binPrimCode f = mconcat ["case (p0, p1) of (S [T x],S [T y]) -> S [\"result \", S [T (show (", f, " (read x) (read y)))]]; _ -> S [\"Don't know\"]"]
