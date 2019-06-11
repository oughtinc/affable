{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Scripting ( Script, RoleId, TemplateId, RQAReply(..), rqaScript, feScript ) where
import Control.Concurrent.STM ( atomically ) -- stm
import Control.Concurrent.STM.TChan ( TChan, newTChanIO, readTChan, writeTChan ) -- stm
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO, putTMVar, takeTMVar ) -- stm
import Data.Aeson ( FromJSON, Value, object, (.=) ) -- aeson
import Data.Traversable ( forM ) -- base
import qualified Data.Text as T -- text
import GHC.Generics ( Generic ) -- ghc

import Message ( Message(..) )
import Scheduler ( SchedulerContext, normalize )

type RoleId = T.Text
type TemplateId = T.Text

-- TODO: Add release :: user -> Score -> IO ()
type Script user d reply a = (RoleId -> IO user) -> (user -> IO ()) -> (user -> TemplateId -> d -> IO reply) -> IO a

data RQAReply = Questions [Message] | Answer Message deriving ( Show, Generic )

instance FromJSON RQAReply

rqaScript :: SchedulerContext extra -> Message -> Script user Value RQAReply Message
rqaScript ctxt qTop newUser release interact = rqaInit
    where userRole = "userRole" :: RoleId
          rqaTemplate = "rqa_template" :: TemplateId
          rqaInit = do
            u <- newUser userRole
            r <- interact u rqaTemplate (object ["question" .= qTop])
            release u
            rqa qTop [] r
          
          rqa q oldQs (Answer a) = normalize ctxt a
          rqa q oldQs (Questions subQs) = do
            subQs <- forM subQs (normalize ctxt)
            subAnswers <- forM subQs $ \subQ -> do -- TODO: Not currently doing this concurrently for simplicity... 
                            subUser <- newUser userRole
                            subResponse <- interact subUser rqaTemplate (object ["question" .= subQ])
                            release subUser
                            rqa subQ [] subResponse

            u <- newUser userRole
            let newQs = oldQs ++ zip subQs subAnswers
            r' <- interact u rqaTemplate (object ["question" .= q, "subQuestions" .= newQs])
            release u
            rqa q newQs r'

feScript :: SchedulerContext extra -> Message -> Script user Value RQAReply Message
feScript ctxt qTop newUser release interact = feInit
    where judgeRole = "judgeRole" :: RoleId
          honestRole = "honestExpert" :: RoleId
          maliciousRole = "maliciousExpert" :: RoleId
          judgeTemplate = "judge_template" :: TemplateId
          honestTemplate = "honest_template" :: TemplateId
          maliciousTemplate = "malicious_template" :: TemplateId

          feInit = do
            ho <- newUser honestRole
            Answer ha <- interact ho honestTemplate (object ["question" .= qTop])
            ha <- normalize ctxt ha
            mo <- newUser maliciousRole
            Answer ma <- interact mo maliciousTemplate (object ["question" .= qTop, "honest_answer" .= ha])
            ma <- normalize ctxt ma
            fe qTop [] ha ma ho mo
          
          fe q oldQs ha ma ho mo = do
            u <- newUser judgeRole
            r <- interact u judgeTemplate (object ["question" .= q, "honest_answer" .= ha, "malicious_answer" .= ma, "subQuestions" .= oldQs])
            release u
            case r of
                Questions subQs -> do
                    subQs <- forM subQs (normalize ctxt)
                    subAnswers <- forM subQs $ \subQ -> do -- TODO: Not currently doing this concurrently for simplicity... 
                                    Answer subHA <- interact ho honestTemplate (object ["question" .= subQ])
                                    subHA <- normalize ctxt subHA
                                    Answer subMA <- interact mo maliciousTemplate (object ["question" .= subQ, "honest_answer" .= subHA])
                                    subMA <- normalize ctxt subMA
                                    fe subQ [] subHA subMA ho mo

                    let newQs = oldQs ++ zip subQs subAnswers
                    fe q newQs ha ma ho mo
                Answer a -> normalize ctxt a
