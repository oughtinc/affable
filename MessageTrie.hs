{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module MessageTrie ( MessageTrie, emptyTrie, match, insert ) where
import qualified Data.Map as M -- containers
import Data.Text ( Text ) -- text

import Message ( Message(..), Pointer, Address, PointerEnvironment )

-- Private
data Step
    = T Text
    | P Pointer -- behaves like a variable
    | L Address
    | OPEN
    | CLOSE
  deriving ( Eq, Ord, Show )    

-- Abstract
newtype MessageTrie a = MessageTrie (M.Map Step (MessageTrie a, Maybe a))
    deriving ( Show ) -- for debugging only

unMessageTrie :: MessageTrie a -> M.Map Step (MessageTrie a, Maybe a)
unMessageTrie (MessageTrie t) = t

emptyTrie :: MessageTrie a
emptyTrie = MessageTrie M.empty

-- Find the most general Message that matches the given Message. Return bindings to all pointers
-- in the general Message to produce the given Message, i.e. a substitution a la unification, subsumption.
-- To do a variant match, require bindings to only bind to other pointers.
match :: Message -> MessageTrie a -> Maybe (PointerEnvironment, a)
match msg (MessageTrie mapping) = go msg mapping M.empty
    where go (Text t) mapping env = undefined
          go (Reference p) mapping env = undefined
          go (Location a) mapping env = undefined
          go (Structured ms) mapping env = undefined

-- Inserting "foo $1 $1" should match against "foo [a] [a]" but not "foo [a] [b]".
insert :: Message -> a -> MessageTrie a -> MessageTrie a    
insert msg v trie = goTop msg (unMessageTrie trie)
    where goTop (Structured ms) mapping = MessageTrie $ M.insert OPEN (subTrie, Nothing) mapping
            where !subTrie = goList ms (insertLeaf CLOSE) $ maybe M.empty (unMessageTrie . fst) $ M.lookup OPEN mapping
          goTop m mapping = insertLeaf (toStep m) mapping

          goList [] k mapping = k mapping
          goList (Structured ms':ms) k mapping = MessageTrie $ M.insertWith (\(t, _) (_, ov) -> (t, ov)) OPEN (subTrie, Nothing) mapping
            where restK mapping' = MessageTrie $ M.insertWith (\(t, _) (_, ov) -> (t, ov)) CLOSE (subSubTrie, Nothing) mapping'
                    where !subSubTrie = goList ms k $ maybe M.empty (unMessageTrie . fst) $ M.lookup CLOSE mapping'
                  !subTrie = goList ms' restK $ maybe M.empty (unMessageTrie . fst) $ M.lookup OPEN mapping
          goList (m:ms) k mapping = MessageTrie $ M.insertWith (\(t, _) (_, ov) -> (t, ov)) s (subTrie, Nothing) mapping
            where !subTrie = goList ms k $ maybe M.empty (unMessageTrie . fst) $ M.lookup s mapping
                  !s = toStep m

          insertLeaf step = MessageTrie . M.insertWith (\(_, nv) (t, _) -> (t, nv)) step (emptyTrie, Just v)

toStep :: Message -> Step
toStep (Text t) = T t
toStep (Reference p) = P p
toStep (Location a) = L a
toStep (Structured _) = OPEN
