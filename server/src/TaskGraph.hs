{-# LANGUAGE OverloadedStrings #-}

module TaskGraph (
    TaskGraph
  , Relation (..)
  , Edge (..)
  , fetchTaskTree
  , merge
  ) where

import Asana (
    Task (..)
  , getTasksForProject
  , getSubtasks
  )
import Control.Monad
import Data.Aeson

data Relation = Subtask | Dependency
  deriving Show
instance ToJSON Relation where
  toJSON Subtask    = "subtask" :: Value
  toJSON Dependency = "dependency" :: Value

data Edge = Edge Relation String String
  deriving Show
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
fetchTaskTree :: String -> Task -> IO TaskGraph
fetchTaskTree token task = do
  subtasks <- getSubtasks token $ taskId task
  let edges = map (makeEdge Subtask task) subtasks
  foldr merge ([task], edges) <$> mapM (fetchTaskTree token) subtasks
