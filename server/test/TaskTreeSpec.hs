module TaskTreeSpec (spec) where

import Test.Hspec
import TaskTree
import TaskGraph (TaskGraph, Edge (..), Relation (..))
import Asana (Task (..))

testGraph :: TaskGraph
testGraph = ([ Task "r"   "Root"
             , Task "a00" "Task A00"
             , Task "a01" "Task A01"
             , Task "a02" "Task A02"
             , Task "a10" "Task A10"
             , Task "b00" "Task B00"
             , Task "b01" "Task B01"
             , Task "c00" "Task C00"
             ],
             [ Edge Subtask "r"   "a00"
             , Edge Subtask "r"   "b00"
             , Edge Subtask "r"   "c00"
             , Edge Subtask "a00" "a01"
             , Edge Subtask "a00" "a02"
             , Edge Subtask "b00" "b01"
             , Edge Subtask "a01" "a10"
             ])

unsoundGraph :: TaskGraph
unsoundGraph = ([ Task "r"   "Root"
                , Task "a00" "Task A00"
                , Task "a01" "Task A01"
                , Task "a10" "Task A10"
                , Task "b00" "Task B00"
                , Task "b01" "Task B01"
                , Task "c00" "Task C00"
                ],
                [ Edge Subtask "r"   "a00"
                , Edge Subtask "r"   "b00"
                , Edge Subtask "r"   "c00"
                , Edge Subtask "a00" "a01"
                , Edge Subtask "a00" "a02"
                , Edge Subtask "b00" "b01"
                , Edge Subtask "a01" "a10"
                ])

doubGraph :: TaskGraph
doubGraph = ([ Task "r"   "Root"
             , Task "a00" "Task A00"
             , Task "a01" "Task A01"
             , Task "a02" "Task A02"
             ],
             [ Edge Subtask "r"   "a00"
             , Edge Subtask "r"   "a01"
             , Edge Subtask "a01" "a02"
             , Edge Subtask "r"   "a02"
             ])

spec :: Spec
spec = describe "Hierarchical TaskGraph implements toTree which" $ do
  it "correctly transforms it into a TaskTree" $
    shouldBe (toTree testGraph "r") $ Just $
      TaskTreeNode "r" "Root" [ TaskTreeNode "a00" "Task A00" [ TaskTreeNode "a01" "Task A01" [ TaskTreeNode "a10" "Task A10" []]
                                                              , TaskTreeNode "a02" "Task A02" []
                                                              ]
                              , TaskTreeNode "b00" "Task B00" [ TaskTreeNode "b01" "Task B01" [] ]
                              , TaskTreeNode "c00" "Task C00" []
                              ]
  it "takes the first child relationship if there are multiple" $ pendingWith "Need to use a HashMap to track instances of Tasks for this to work, current algorithm is too naïve"
  it "returns Nothing if the graph is missing tasks for which edges exist" $
    shouldBe (toTree unsoundGraph "r") Nothing
  it "returns Nothing if the root node is missing" $
    shouldBe (toTree testGraph "s") Nothing
