{-# LANGUAGE OverloadedStrings #-}
module SqliteCompletionContext ( makeSqliteCompletionContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Text as T -- text
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, queryNamed ) -- sqlite-simple

import Completions ( CompletionContext(..) )
import Exp ( Pattern )
import Message ( generalizeMessage, stripLabel, parsePatternsUnsafe )
import Scheduler ( SchedulerContext(..), SessionId )
import Util ( Lock, withLock )

preparePattern :: T.Text -> Pattern
preparePattern = snd . generalizeMessage 0 . stripLabel . head . parsePatternsUnsafe

makeSqliteCompletionContext :: SchedulerContext (Connection, Lock) -> IO (CompletionContext (Connection, Lock))
makeSqliteCompletionContext ctxt = do
    let (conn, lock) = extraContent ctxt

    primPatterns <- map (\(Only t) -> preparePattern (T.concat ["[", t, "]"])) <$> query_ conn "SELECT pattern FROM Primitives"

    return $ CompletionContext {
                    completionsFor = completionsForSqlite lock conn primPatterns,
                    schedulerContext = ctxt
                }

-- NOT CACHEABLE
completionsForSqlite :: Lock -> Connection -> [Pattern] -> SessionId -> IO [Pattern]
completionsForSqlite lock conn primPatterns sessionId = do
    withLock lock $ do
        [fId] <- queryNamed conn "SELECT f.id \
                                 \FROM Functions f \
                                 \INNER JOIN Continuations c ON c.function = f.id \
                                 \INNER JOIN Trace t ON t.workspaceId = c.workspaceId \
                                 \INNER JOIN SessionProcesses s ON s.processId = t.processId \
                                 \WHERE s.sessionId = :sessionId AND f.isAnswer = 1 \
                                 \LIMIT 1" [":sessionId" := sessionId]

        alts <- query conn "SELECT pattern FROM Alternatives WHERE function = ?" (fId :: Only Int64)
        return $ primPatterns ++ map (\(Only ps) -> preparePattern ps) alts