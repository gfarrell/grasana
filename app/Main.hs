{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dot (Renderable (toDot))
import Html (makeHtml)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (ByteString, fromString, toString)
import Asana
import Control.Concurrent.QSem
import Control.Concurrent.Async (mapConcurrently)
import Data.Functor ((<&>))
import System.Environment (getEnv, getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))
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

unsoundGraphHTML :: ByteString
unsoundGraphHTML = fromString "<!DOCTYPE html><html><body><p>unsound graph</p></body></html>"

-- TODO: using MaybeT or similar MTS to handle the error state and exit with an
-- error code (for both unknown actions and unsound graphs).

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage m e = hPutStrLn stderr ("Error: " ++ m) >> exitWith e

usage = do
  putStrLn "Usage: grasana [-h] [-t token] format projectid"
  putStrLn ""
  putStrLn "  - token: optional Asana personal access token, if not specified will be expected in the environment as ASANA_PAT"
  putStrLn "  - format: html | dot | jsongraph | jsontree"
  putStrLn "  - projectid: the ID of the Asana project you want to transform into a graph"
  putStrLn ""

parse :: [String] -> IO ()
parse ("-h":_) = usage >> exitSuccess
parse ["-t", token, a, p] = parse2 [a, p] token
parse ("-t":_) = usage >> exitWithErrorMessage "missing token" (ExitFailure 3)
parse args = getEnv "ASANA_PAT" >>= parse2 args

parse2 :: [String] -> String -> IO ()
parse2 ["dot", p] t       = getTaskGraph t p >>= putStrLn . toDot >> exitSuccess
parse2 ["jsongraph", p] t = getTaskGraph t p >>= putStrLn . toString . encode >> exitSuccess
parse2 ["jsontree", p] t  = getTaskTree t p >>=
                              putStrLn . toString . maybe unsoundGraphJSON encode
                              >> exitSuccess
parse2 ["html", p] t      = getTaskTree t p >>=
                              putStrLn . toString . maybe unsoundGraphHTML (fromString . makeHtml)
                              >> exitSuccess
parse2 [a, _] _           = exitWithErrorMessage ("unknown action " ++ a) (ExitFailure 2)
parse2 _ _                = usage >> exitWithErrorMessage "missing arguments" (ExitFailure 1)

main :: IO ()
main = getArgs >>= parse
