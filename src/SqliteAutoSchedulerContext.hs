{-# LANGUAGE OverloadedStrings #-}
module SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext, makeSqliteAutoSchedulerContext' ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, executeNamed, execute_, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..) )
import Exp ( Pattern, Exp(..), Exp', Name(..), expToBuilderDB, expFromDB )
import Message ( messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..) )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText, Lock, withLock )

type FunctionId = Int64

makeSqliteAutoSchedulerContext :: Connection -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext conn = do
    ctxt <- makeSqliteSchedulerContext conn
    makeSqliteAutoSchedulerContext' ctxt

makeSqliteAutoSchedulerContext' :: SchedulerContext (Connection, Lock) -> IO (AutoSchedulerContext (Connection, Lock))
makeSqliteAutoSchedulerContext' ctxt = do
    let (conn, lock) = extraContent ctxt

    answerId <- withLock lock $ do
        execute_ conn "INSERT INTO Functions(isAnswer) VALUES (1)"
        lastInsertRowId conn

    return $ AutoSchedulerContext {
                    alternativesFor = alternativesForSqlite lock conn answerId,
                    allAlternatives = allAlternativesSqlite lock conn answerId,
                    addCaseFor = addCaseForSqlite lock conn answerId,
                    newFunction = newFunctionSqlite lock conn,
                    schedulerContext = ctxt
                }

nameToId :: FunctionId -> Name -> FunctionId
nameToId answerId ANSWER = answerId
nameToId        _ (LOCAL i) = fromIntegral i

idToName :: FunctionId -> FunctionId -> Name
idToName answerId fId | answerId == fId = ANSWER
                      | otherwise = LOCAL (fromIntegral fId)

alternativesForSqlite :: Lock -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForSqlite lock conn answerId f = do
    withLock lock $ do
        let !fId = nameToId answerId f
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

allAlternativesSqlite :: Lock -> Connection -> FunctionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesSqlite lock conn answerId = do
    withLock lock $ do
        alts <- query_ conn "SELECT function, pattern, body FROM Alternatives"
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

newFunctionSqlite :: Lock -> Connection -> IO Name
newFunctionSqlite lock conn = do
    withLock lock $ do
        execute_ conn "INSERT INTO Functions DEFAULT VALUES"
        (LOCAL . fromIntegral) <$> lastInsertRowId conn

addCaseForSqlite :: Lock -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForSqlite lock conn answerId f patterns e = do
    withLock lock $ do
        let !fId = nameToId answerId f
        executeNamed conn "INSERT INTO Alternatives (function, pattern, body) VALUES (:function, :patterns, :body)" [
                            ":function" := fId,
                            ":patterns" := toText (patternsToBuilder patterns),
                            ":body" := toText (expToBuilderDB e)]
