{-# LANGUAGE OverloadedStrings #-}
module Sqlite.CompletionContext ( makeSqliteCompletionContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Text as T -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, queryNamed ) -- sqlite-simple

import Completions ( CompletionContext(..), preparePattern )
import Exp ( Pattern )
import Scheduler ( SchedulerContext(..), SessionId, sessionIdToBuilder )
import Util ( Queue, enqueueSync, toText )

makeSqliteCompletionContext :: SchedulerContext (Connection, Queue) -> IO (CompletionContext (Connection, Queue))
makeSqliteCompletionContext ctxt = do
    let (conn, q) = extraContent ctxt

    primPatterns <- enqueueSync q $ do
        map (\(Only t) -> preparePattern (T.concat ["[", t, "]"])) <$> query_ conn "SELECT pattern FROM Primitives"

    return $ CompletionContext {
                    completionsFor = completionsForSqlite q conn primPatterns,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForSqlite :: Queue -> Connection -> [Pattern] -> SessionId -> IO [Pattern]
completionsForSqlite q conn primPatterns sessionId = do
    enqueueSync q $ do
        [fId] <- queryNamed conn "SELECT f.id \
                                 \FROM Functions f \
                                 \INNER JOIN Continuations c ON c.function = f.id \
                                 \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                 \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                 \WHERE s.sessionId = :sessionId AND f.isAnswer = 1 \
                                 \LIMIT 1" [":sessionId" := toText (sessionIdToBuilder sessionId)]

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ primPatterns ++ map (\(Only ps) -> preparePattern ps) alts
