{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Sqlite.CompletionContext ( makeSqliteCompletionContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Text as T -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, queryNamed ) -- sqlite-simple

import Completions ( CompletionContext(..), preparePattern )
import Exp ( Pattern )
import Scheduler ( SchedulerContext(..), SessionId, SyncFunc, sessionIdToBuilder )
import Util ( Queue, toText )

makeSqliteCompletionContext :: SchedulerContext (Connection, Queue) -> IO (CompletionContext (Connection, Queue))
makeSqliteCompletionContext ctxt = do
    let (conn, q) = extraContent ctxt
    let sync = doAtomically ctxt

    primPatterns <- sync $ do
        map (\(Only t) -> preparePattern (T.concat ["[", t, "]"])) <$> query_ conn "SELECT pattern FROM Primitives"

    return $ CompletionContext {
                    completionsFor = completionsForSqlite sync q conn primPatterns,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForSqlite :: SyncFunc -> Queue -> Connection -> [Pattern] -> SessionId -> IO [Pattern]
completionsForSqlite sync q conn primPatterns sessionId = do
    sync $ do
        [fId] <- queryNamed conn "SELECT f.id \
                                 \FROM Functions f \
                                 \INNER JOIN Continuations c ON c.function = f.id \
                                 \INNER JOIN Trace t ON t.versionId = c.versionId \
                                 \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                 \WHERE s.sessionId = :sessionId AND f.isAnswer = 1 \
                                 \LIMIT 1" [":sessionId" := toText (sessionIdToBuilder sessionId)]

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ primPatterns ++ map (\(Only ps) -> preparePattern ps) alts
