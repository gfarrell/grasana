export interface Task {
  gid: string;
  name: string;
}
export type EdgeType = "subtask" | "dependency";
export type Edge = [EdgeType, string, string];
export type TaskGraph = [Task[], Edge[]];

export type TaskTree = {
  id: string;
  name: string;
  children: TaskTree[];
};

import * as td from "./test-data.json";
import * as tt from "./test-tree.json";

export async function fetchProjectGraph(projectId: string): Promise<TaskGraph> {
  return td as TaskGraph;
}

export async function fetchProjectTree(projectId: string): Promise<TaskTree> {
  return tt as TaskTree;
}
