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
