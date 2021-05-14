import React from "react";
import ReactDOM from "react-dom";

import * as td from "./test-data.json";

interface Task {
  gid: string;
  name: string;
}
type EdgeType = "subtask" | "dependency";
type Edge = [EdgeType, string, string];
type TaskGraph = [Task[], Edge[]];

interface ForceNode {
  id: string;
  name: string;
  influence: number;
}
interface ForceEdge {
  source: string;
  target: string;
  weight: number;
  type: EdgeType;
}
interface ForceGraph {
  nodes: ForceNode[];
  links: ForceEdge[];
}

const taskToForceNode = (t: Task): ForceNode => ({
  id: t.gid,
  name: t.name,
  influence: 1,
});
const edgeToForceEdge = (e: Edge): ForceEdge => ({
  source: e[1],
  target: e[2],
  type: e[0],
  weight: e[0] == "subtask" ? 3 : 2,
});

const prepareData = (g: TaskGraph): ForceGraph => ({
  nodes: g[0].map(taskToForceNode),
  links: g[1].map(edgeToForceEdge),
});

const TaskGraphVisualiser: React.FunctionComponent<{ graph: TaskGraph }> = ({
  graph,
}) => {
  const forceGraph = prepareData(graph);
  return <svg className="TaskGraphVisualiser"></svg>;
};

ReactDOM.render(<TaskGraphVisualiser graph={ td as TaskGraph } />, document.getElementById("AppContainer"));
