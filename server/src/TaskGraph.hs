{-# LANGUAGE OverloadedStrings #-}

module TaskGraph (
    TaskGraph
  , Relation (..)
  , Edge (..)
  , rFetchTaskGraph
  , merge
  ) where

import Asana (
    Task (..)
  , getTasksForProject
  , getSubtasks
  )
import Control.Monad
import Data.Aeson
import Data.Vector ((!))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem
import Control.Exception (bracket_)

data Relation = Subtask | Dependency
  deriving Show
instance ToJSON Relation where
  toJSON Subtask    = "subtask" :: Value
  toJSON Dependency = "dependency" :: Value

data Edge = Edge {
  rel :: Relation
, parentId :: String
, childId :: String
} deriving Show
instance ToJSON Edge where
  toJSON (Edge rel parentId childId) = toJSON (rel, parentId, childId)

type TaskGraph = ([Task], [Edge])

merge :: TaskGraph -> TaskGraph -> TaskGraph
merge (aTasks, aEdges) (bTasks, bEdges) = (aTasks ++ bTasks, aEdges ++ bEdges)

makeEdge :: Relation -> Task -> Task -> Edge
makeEdge rel parent child = Edge rel (taskId parent) (taskId child)

-- | Fetches the tree of tasks with a given task at the root and returns a
-- TaskGraph (i.e. a list of Tasks and a list of Edges).
-- TODO: integrate dependencies as well as subtasks
-- TODO: handle circular dependencies
rFetchTaskGraph :: QSem -> String -> Task -> IO TaskGraph
rFetchTaskGraph sem token task =
    do
      subtasks <- bracket_ (waitQSem sem) (signalQSem sem) $ getSubtasks token $ taskId task
      let edges = map (makeEdge Subtask task) subtasks
      foldr merge ([task], edges) <$> mapConcurrently (rFetchTaskGraph sem token) subtasks
