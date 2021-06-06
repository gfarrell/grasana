{-# LANGUAGE FlexibleInstances #-}
module TaskTree (
  TaskTreeNode (..)
, Hierarchical (..)
) where

import TaskGraph (TaskGraph, Edge(parentId, childId))
import Asana (Task(taskId, taskName))
import Data.HashMap (Map(..), (!), insert, empty, notMember)

data TaskTreeNode = TaskTreeNode {
  nodeId :: String
, nodeName :: String
, children :: [TaskTreeNode]
  } deriving (Eq, Show)

class Hierarchical a where
  toTree :: a -> String -> Maybe TaskTreeNode

getTask :: [Task] -> String -> Maybe Task
getTask tasks id = let found = filter ((== id) . taskId) tasks
                    in case length found of 1 -> Just $ head found
                                            _ -> Nothing

makeTreeNode :: TaskGraph -> Task -> Maybe TaskTreeNode
makeTreeNode graph@(ts, es) t =
  sequence (getTask ts . childId <$> filter ((== taskId t) . parentId) es)
    >>= \childTasks -> sequence (makeTreeNode graph <$> childTasks)
      >>= \childNodes -> return $ TaskTreeNode (taskId t) (taskName t) childNodes

instance Hierarchical TaskGraph where
  toTree graph rootId = getTask (fst graph) rootId >>= \rootTask -> makeTreeNode graph rootTask
