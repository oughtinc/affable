{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Postgres.CompletionContext ( makePostgresCompletionContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Text as T -- text
import Database.PostgreSQL.Simple ( Connection, Only(..), query, query_ ) -- postgresql-simple

import Completions ( CompletionContext(..), preparePattern )
import Exp ( Pattern )
import Scheduler ( SchedulerContext(..), SessionId, SyncFunc )
import Util ( Queue )

makePostgresCompletionContext :: SchedulerContext (Connection, Queue) -> IO (CompletionContext (Connection, Queue))
makePostgresCompletionContext ctxt = do
    let (conn, q) = extraContent ctxt
    let sync = doAtomically ctxt

    primPatterns <- sync $ do
        map (\(Only t) -> preparePattern (T.concat ["[", t, "]"])) <$> query_ conn "SELECT pattern FROM Primitives"

    return $ CompletionContext {
                    completionsFor = completionsForPostgres sync q conn primPatterns,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForPostgres :: SyncFunc -> Queue -> Connection -> [Pattern] -> SessionId -> IO [Pattern]
completionsForPostgres sync q conn primPatterns sessionId = do
    sync $ do
        [fId] <- query conn "SELECT f.id \
                            \FROM Functions f \
                            \INNER JOIN Continuations c ON c.function = f.id \
                            \INNER JOIN Trace t ON t.versionId = c.versionId \
                            \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                            \WHERE s.sessionId = ? AND f.isAnswer = 1 \
                            \LIMIT 1" (Only sessionId)

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ primPatterns ++ map (\(Only ps) -> preparePattern ps) alts
