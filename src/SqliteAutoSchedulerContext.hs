{-# LANGUAGE OverloadedStrings #-}
module SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext', makeSqliteAutoSchedulerContext ) where
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, executeNamed, execute_, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..) )
import Exp ( Pattern, Exp(..), Exp', Name(..), expToBuilderDB, expFromDB )
import Message ( messageToBuilder, parseMessageUnsafeDB )
import Scheduler ( SchedulerContext(..) )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText )

type FunctionId = Int64

makeSqliteAutoSchedulerContext' :: Connection -> SchedulerContext extra -> IO (AutoSchedulerContext extra)
makeSqliteAutoSchedulerContext' conn ctxt = do
    execute_ conn "INSERT INTO Functions DEFAULT VALUES"
    answerId <- lastInsertRowId conn
    return $ AutoSchedulerContext {
                    alternativesFor = alternativesForSqlite conn answerId,
                    allAlternatives = allAlternativesSqlite conn answerId,
                    addCaseFor = addCaseForSqlite conn answerId,
                    newFunction = newFunctionSqlite conn,
                    schedulerContext = ctxt
                }

makeSqliteAutoSchedulerContext :: Connection -> IO (AutoSchedulerContext Connection)
makeSqliteAutoSchedulerContext conn = makeSqliteAutoSchedulerContext' conn =<< makeSqliteSchedulerContext conn

nameToId :: FunctionId -> Name -> FunctionId
nameToId answerId ANSWER = answerId
nameToId        _ (LOCAL i) = fromIntegral i

idToName :: FunctionId -> FunctionId -> Name
idToName answerId fId | answerId == fId = ANSWER
                      | otherwise = LOCAL (fromIntegral fId)

alternativesForSqlite :: Connection -> FunctionId -> Name -> IO [(Pattern, Exp')]
alternativesForSqlite conn answerId f = do
    let !fId = nameToId answerId f
    alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
    return $ map (\(p, e) -> (parseMessageUnsafeDB p, expFromDB e)) alts

allAlternativesSqlite :: Connection -> FunctionId -> IO (M.Map Name [(Pattern, Exp')])
allAlternativesSqlite conn answerId = do
    alts <- query_ conn "SELECT function, pattern, body FROM Alternatives"
    return $ M.fromListWith (++) $ map (\(f, p, e) -> (idToName answerId f, [(parseMessageUnsafeDB p, expFromDB e)])) alts

newFunctionSqlite :: Connection -> IO Name
newFunctionSqlite conn = do
    execute_ conn "INSERT INTO Functions DEFAULT VALUES"
    (LOCAL . fromIntegral) <$> lastInsertRowId conn

addCaseForSqlite :: Connection -> FunctionId -> Name -> Pattern -> Exp' -> IO ()
addCaseForSqlite conn answerId f pattern e = do
    let !fId = nameToId answerId f
    executeNamed conn "INSERT INTO Alternatives (function, pattern, body) VALUES (:function, :pattern, :body)" [
                        ":function" := fId,
                        ":pattern" := toText (messageToBuilder pattern),
                        ":body" := toText (expToBuilderDB e)]
