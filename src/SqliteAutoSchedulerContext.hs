{-# LANGUAGE OverloadedStrings #-}
module SqliteAutoSchedulerContext ( makeSqliteAutoSchedulerContext ) where
import Control.Exception ( bracket_ ) -- base
import Data.Int ( Int64 ) -- base
import qualified Data.Map as M -- containers
import Database.SQLite.Simple ( Connection, Only(..), NamedParam(..), query, query_, executeNamed, execute_, lastInsertRowId ) -- sqlite-simple

import AutoScheduler ( AutoSchedulerContext(..) )
import Exp ( Pattern, Exp(..), Exp', Name(..), expToBuilderDB, expFromDB )
import Message ( messageToBuilder, parseMessageUnsafeDB, parsePatternsUnsafe, patternsToBuilder )
import Scheduler ( SchedulerContext(..) )
import SqliteSchedulerContext ( makeSqliteSchedulerContext )
import Util ( toText )

type FunctionId = Int64

makeSqliteAutoSchedulerContext :: Connection -> IO (AutoSchedulerContext (Connection, IO (), IO ()))
makeSqliteAutoSchedulerContext conn = do
    ctxt <- makeSqliteSchedulerContext conn
    let (_, lock, unlock) = extraContent ctxt

    answerId <- bracket_ lock unlock $ do
        execute_ conn "INSERT INTO Functions(isAnswer) VALUES(1)"
        lastInsertRowId conn

    return $ AutoSchedulerContext {
                    alternativesFor = alternativesForSqlite lock unlock conn answerId,
                    allAlternatives = allAlternativesSqlite lock unlock conn answerId,
                    addCaseFor = addCaseForSqlite lock unlock conn answerId,
                    newFunction = newFunctionSqlite lock unlock conn,
                    schedulerContext = ctxt
                }

nameToId :: FunctionId -> Name -> FunctionId
nameToId answerId ANSWER = answerId
nameToId        _ (LOCAL i) = fromIntegral i

idToName :: FunctionId -> FunctionId -> Name
idToName answerId fId | answerId == fId = ANSWER
                      | otherwise = LOCAL (fromIntegral fId)

alternativesForSqlite :: IO () -> IO () -> Connection -> FunctionId -> Name -> IO [([Pattern], Exp')]
alternativesForSqlite lock unlock conn answerId f = do
    bracket_ lock unlock $ do
        let !fId = nameToId answerId f
        alts <- query conn "SELECT pattern, body FROM Alternatives WHERE function = ?" (Only fId)
        return $ map (\(ps, e) -> (parsePatternsUnsafe ps, expFromDB e)) alts

allAlternativesSqlite :: IO () -> IO () -> Connection -> FunctionId -> IO (M.Map Name [([Pattern], Exp')])
allAlternativesSqlite lock unlock conn answerId = do
    bracket_ lock unlock $ do
        alts <- query_ conn "SELECT function, pattern, body FROM Alternatives"
        return $ M.fromListWith (++) $ map (\(f, ps, e) -> (idToName answerId f, [(parsePatternsUnsafe ps, expFromDB e)])) alts

newFunctionSqlite :: IO () -> IO () -> Connection -> IO Name
newFunctionSqlite lock unlock conn = do
    bracket_ lock unlock $ do
        execute_ conn "INSERT INTO Functions DEFAULT VALUES"
        (LOCAL . fromIntegral) <$> lastInsertRowId conn

addCaseForSqlite :: IO () -> IO () -> Connection -> FunctionId -> Name -> [Pattern] -> Exp' -> IO ()
addCaseForSqlite lock unlock conn answerId f patterns e = do
    bracket_ lock unlock $ do
        let !fId = nameToId answerId f
        executeNamed conn "INSERT INTO Alternatives (function, pattern, body) VALUES (:function, :patterns, :body)" [
                            ":function" := fId,
                            ":patterns" := toText (patternsToBuilder patterns),
                            ":body" := toText (expToBuilderDB e)]
