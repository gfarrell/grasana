{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TaskTree (
  TaskTreeNode (..)
, Hierarchical (..)
) where

import TaskGraph (TaskGraph, Edge(parentId, childId))
import Asana (Task(taskId, taskName))
import Data.Aeson
import Data.Functor ((<&>))

data TaskTreeNode = TaskTreeNode {
  nodeId :: String
, nodeName :: String
, children :: [TaskTreeNode]
  } deriving (Eq, Show)

instance ToJSON TaskTreeNode where
  toJSON (TaskTreeNode id name children) = object [ "id" .= id, "name" .= name, "children" .= toJSON children ]

class Hierarchical a where
  toTree :: a -> String -> Maybe TaskTreeNode

getTask :: [Task] -> String -> Maybe Task
getTask tasks id = let found = filter ((== id) . taskId) tasks
                    in case length found of 1 -> Just $ head found
                                            _ -> Nothing

makeTreeNode :: TaskGraph -> Task -> Maybe TaskTreeNode
makeTreeNode graph@(ts, es) t =
  mapM (getTask ts . childId) (filter ((== taskId t) . parentId) es) >>= mapM (makeTreeNode graph)
  <&> TaskTreeNode (taskId t) (taskName t)

instance Hierarchical TaskGraph where
  toTree graph rootId = getTask (fst graph) rootId >>= makeTreeNode graph
