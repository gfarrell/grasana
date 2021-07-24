{-# LANGUAGE FlexibleInstances #-}

module Dot (
  Renderable (..)
) where

import Asana (Task (taskId, taskName))
import TaskGraph (TaskGraph, Edge(parentId, childId))
import Data.Text (Text, pack, unpack)
import Text.Wrap (wrapText, defaultWrapSettings)

class Renderable a where
  toDot :: a -> String

repeatStr :: Integer -> String -> String
repeatStr 0 s = s
repeatStr i s = s ++ repeatStr (i - 1) s

mkLine :: Integer -> String -> String
mkLine i s = repeatStr i "  " ++ s ++ "\n"

wrap :: String -> String
wrap = unpack . wrapText defaultWrapSettings 24 . pack

instance Renderable Edge where
  toDot x = parentId x ++ " -> " ++ childId x

instance Renderable Task where
  toDot x = taskId x ++ " [label=\"" ++ (wrap . taskName $ x) ++ "\"]"

instance Renderable TaskGraph where
  toDot (ts, es) =   mkLine 0 "digraph Project {"
                  ++ mkLine 1 "rankdir=LR"
                  ++ concatMap (mkLine 1 . toDot) ts
                  ++ concatMap (mkLine 1 . toDot) es
                  ++ mkLine 0 "}"
