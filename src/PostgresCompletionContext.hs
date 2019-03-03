{-# LANGUAGE OverloadedStrings #-}
module PostgresCompletionContext ( makePostgresCompletionContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Text as T -- text
import Database.PostgreSQL.Simple ( Connection, Only(..), query, query_ ) -- postgresql-simple

import Completions ( CompletionContext(..), preparePattern )
import Exp ( Pattern )
import Scheduler ( SchedulerContext(..), SessionId )
import Util ( Lock, withLock )

makePostgresCompletionContext :: SchedulerContext (Connection, Lock) -> IO (CompletionContext (Connection, Lock))
makePostgresCompletionContext ctxt = do
    let (conn, lock) = extraContent ctxt

    primPatterns <- map (\(Only t) -> preparePattern (T.concat ["[", t, "]"])) <$> query_ conn "SELECT pattern FROM Primitives"

    return $ CompletionContext {
                    completionsFor = completionsForPostgres lock conn primPatterns,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForPostgres :: Lock -> Connection -> [Pattern] -> SessionId -> IO [Pattern]
completionsForPostgres lock conn primPatterns sessionId = do
    withLock lock $ do
        [fId] <- query conn "SELECT f.id \
                            \FROM Functions f \
                            \INNER JOIN Continuations c ON c.function = f.id \
                            \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                            \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                            \WHERE s.sessionId = ? AND f.isAnswer = 1 \
                            \LIMIT 1" (Only sessionId)

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ primPatterns ++ map (\(Only ps) -> preparePattern ps) alts
