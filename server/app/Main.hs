{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Yesod
import Data.Aeson (Value)
import Asana (getTasksForProject, Task (..))
import System.Environment (getEnv)
import TaskGraph (rFetchTaskGraph, merge, Edge (..), Relation (..))
import TaskTree
import Network.HTTP.Types ( status500 )
import Control.Concurrent.QSem
import Control.Concurrent.Async

data GrasanaApp = GrasanaApp

mkYesod "GrasanaApp" [parseRoutes|
                     / HomeR GET
                     /projects/#Int ProjectR GET
                     /trees/#Int TreeR GET
                     |]

instance Yesod GrasanaApp

getHomeR = return $ object ["msg" .= "Hello World"]

getProjectR :: Int -> Handler Value
getProjectR projectIdInt = do
  sem   <- liftIO $ newQSem 15 -- limit of 15 requests concurrently
  token <- liftIO $ getEnv "ASANA_PAT"
  tasks <- liftIO $ getTasksForProject token $ show projectIdInt
  returnJson =<< liftIO (foldr merge ([], []) <$> mapConcurrently (rFetchTaskGraph sem token) tasks)

getTreeR :: Int -> Handler Value
getTreeR projectIdInt = do
  let projectId = show projectIdInt
  sem   <- liftIO $ newQSem 30 -- limit of 30 requests concurrently
  token <- liftIO $ getEnv "ASANA_PAT"
  tasks <- liftIO $ getTasksForProject token projectId
  graph <- liftIO $ foldr merge ([ Task projectId "Project Root" ], map (Edge Subtask projectId . taskId) tasks) <$> mapConcurrently (rFetchTaskGraph sem token) tasks
  case toTree graph projectId of Just tree -> returnJson tree
                                 Nothing   -> sendResponseStatus status500 "Unsound graph"

main = warp 1337 GrasanaApp
