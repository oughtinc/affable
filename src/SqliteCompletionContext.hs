{-# LANGUAGE OverloadedStrings #-}
module SqliteCompletionContext ( makeSqliteCompletionContext ) where
import Data.Int ( Int64 ) -- base
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, queryNamed ) -- sqlite-simple

import Completions ( CompletionContext(..) )
import Exp ( Pattern )
import Message ( generalizeMessage, stripLabel, parsePatternsUnsafe )
import Scheduler ( SchedulerContext(..), SessionId )
import Util ( Lock, withLock )

makeSqliteCompletionContext :: SchedulerContext (Connection, Lock) -> IO (CompletionContext (Connection, Lock))
makeSqliteCompletionContext ctxt = do
    let (conn, lock) = extraContent ctxt

    return $ CompletionContext {
                    completionsFor = completionsForSqlite lock conn,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForSqlite :: Lock -> Connection -> SessionId -> IO [Pattern]
completionsForSqlite lock conn sessionId = do
    withLock lock $ do
        [fId] <- queryNamed conn "SELECT f.id \
                                 \FROM Functions f \
                                 \INNER JOIN Continuations c ON c.function = f.id \
                                 \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                 \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                 \WHERE s.sessionId = :sessionId AND f.isAnswer = 1 \
                                 \LIMIT 1" [":sessionId" := sessionId]

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ map (\(Only ps) -> snd $ generalizeMessage 0 $ stripLabel $ head $ parsePatternsUnsafe ps) alts
