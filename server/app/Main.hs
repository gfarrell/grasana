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
  token <- liftIO $ getEnv "ASANA_PAT"
  tasks <- liftIO $ getTasksForProject token $ show projectIdInt
  returnJson =<< liftIO (foldr merge ([], []) <$> mapM (rFetchTaskGraph token) tasks)

getTreeR :: Int -> Handler Value
getTreeR projectIdInt = do
  let projectId = show projectIdInt
  token <- liftIO $ getEnv "ASANA_PAT"
  tasks <- liftIO $ getTasksForProject token projectId
  graph <- liftIO $ foldr merge ([ Task projectId "Project Root" ], map (Edge Subtask projectId . taskId) tasks) <$> mapM (rFetchTaskGraph token) tasks
  case toTree graph projectId of Just tree -> returnJson tree
                                 Nothing   -> sendResponseStatus status500 "Unsound graph"

main = warp 1337 GrasanaApp
