export interface Task {
  gid: string;
  name: string;
}
export type EdgeType = "subtask" | "dependency";
export type Edge = [EdgeType, string, string];
export type TaskGraph = [Task[], Edge[]];

import * as td from "./test-data.json";

export async function fetchProjectGraph(projectId: string): Promise<TaskGraph> {
  return td as TaskGraph;
}
