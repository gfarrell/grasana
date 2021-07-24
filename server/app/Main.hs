{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dot (Renderable (toDot))
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (ByteString, fromString, toString)
import Asana
import Control.Concurrent.QSem
import Control.Concurrent.Async (mapConcurrently)
import Data.Functor ((<&>))
import System.Environment (getEnv, getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)
import TaskTree
import TaskGraph

generateInitialGraph :: String -> [Task] -> TaskGraph
generateInitialGraph projectId tasks = ([ Task projectId "Project Root" ], map (Edge Subtask projectId . taskId) tasks)

getTaskGraph :: String -> String -> IO TaskGraph
getTaskGraph token projectId = do
  sem <- newQSem 16 -- limit to 16 concurrent requests
  getTasksForProject token projectId >>= \t -> fmap (foldr merge (generateInitialGraph projectId t))
                                                  . mapConcurrently (rFetchTaskGraph sem token) $ t

getTaskTree :: String -> String -> IO (Maybe TaskTreeNode)
getTaskTree token projectId = getTaskGraph token projectId <&> flip toTree projectId

unsoundGraphJSON :: ByteString
unsoundGraphJSON = fromString "{ \"error\":\"unsound graph\" }"

-- TODO: using MaybeT or similar MTS to handle the error state and exit with an
-- error code (for both unknown actions and unsound graphs).

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage m e = hPutStrLn stderr m >> exitWith e

runAction :: String -> String -> String -> IO ByteString
runAction "dot" token projectId       = getTaskGraph token projectId <&> fromString . toDot
runAction "jsongraph" token projectId = getTaskGraph token projectId <&> encode
runAction "jsontree" token projectId  = getTaskTree token projectId <&> maybe unsoundGraphJSON encode
-- TODO: enable html output format
-- runAction "html" token projectId      = getTaskTree token projectId <&> fromString . renderHtml
runAction a _ _                       = exitWithErrorMessage ("unknown action: " ++ a) (ExitFailure 1)

main :: IO ()
main = do
  [action, projectId] <- getArgs
  token <- getEnv "ASANA_PAT"
  runAction action token projectId >>= putStr . toString
