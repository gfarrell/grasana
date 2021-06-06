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
instance FromJSON Edge where
  parseJSON j = do
    [r, p, c] <- parseJSON j
    return $ Edge (case r of "subtask" -> Subtask; "dependency" -> Dependency) p c

type TaskGraph = ([Task], [Edge])

merge :: TaskGraph -> TaskGraph -> TaskGraph
merge (aTasks, aEdges) (bTasks, bEdges) = (aTasks ++ bTasks, aEdges ++ bEdges)

makeEdge :: Relation -> Task -> Task -> Edge
makeEdge rel parent child = Edge rel (taskId parent) (taskId child)

-- | Fetches the tree of tasks with a given task at the root and returns a
-- TaskGraph (i.e. a list of Tasks and a list of Edges).
-- TODO: integrate dependencies as well as subtasks
-- TODO: handle circular dependencies
rFetchTaskGraph :: String -> Task -> IO TaskGraph
rFetchTaskGraph token task = do
  subtasks <- getSubtasks token $ taskId task
  let edges = map (makeEdge Subtask task) subtasks
  foldr merge ([task], edges) <$> mapM (rFetchTaskGraph token) subtasks
