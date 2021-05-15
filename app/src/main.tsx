import React from "react";
import ReactDOM from "react-dom";
import {
  forceSimulation,
  Simulation,
  forceManyBody,
  forceLink,
  forceCenter,
  SimulationNodeDatum,
  SimulationLinkDatum,
} from "d3-force";

import * as td from "./test-data.json";

interface Task {
  gid: string;
  name: string;
}
type EdgeType = "subtask" | "dependency";
type Edge = [EdgeType, string, string];
type TaskGraph = [Task[], Edge[]];

interface GraphNode extends SimulationNodeDatum {
  id: string;
  name: string;
  x: number;
  y: number;
}
interface GraphEdge extends SimulationLinkDatum<GraphNode> {
  source: string;
  target: string;
  type: EdgeType;
}

const taskToGraphNode = (t: Task): GraphNode => ({
  id: t.gid,
  name: t.name === "" ? "unnamed" : t.name,
  x: 0,
  y: 0,
});
const edgeToGraphEdge = (e: Edge): GraphEdge => ({
  source: e[1],
  target: e[2],
  type: e[0],
});

interface TaskGraphVisualiserProps {
  graph: TaskGraph;
  width: number;
  height: number;
}

interface TaskGraphVisualiserState {
  nodes: GraphNode[];
  edges: GraphEdge[];
}

class TaskGraphVisualiser extends React.Component<
  TaskGraphVisualiserProps,
  TaskGraphVisualiserState
> {
  width: number;
  height: number;
  svg: SVGSVGElement;
  simulation: Simulation<GraphNode, GraphEdge>;

  constructor(props: TaskGraphVisualiserProps) {
    super(props);
    this.width = props.width;
    this.height = props.height;

    const nodes = props.graph[0].map(taskToGraphNode);
    const edges = props.graph[1].map(edgeToGraphEdge);

    this.simulation = forceSimulation(nodes)
      .force("charge", forceManyBody())
      .force(
        "link",
        forceLink(edges).id((n: GraphNode) => n.id).distance(() => 10)
      )
      .force("center", forceCenter(props.width / 2, props.height / 2));

    this.state = { nodes, edges };
    this.simulation.on("tick", () => this.updateNodePositions());
  }

  updateNodePositions() {
    this.setState({
      nodes: this.simulation.nodes(),
    });
  }

  render() {
    const circles = this.state.nodes.map((n) => (
      <circle
        key={n.id}
        stroke="#CCC"
        strokeWidth={0.5}
        r={20}
        cx={n.x}
        cy={n.y}
      />
    ));
    return (
      <svg
        className="TaskGraphVisualiser"
        ref={(r) => (this.svg = r)}
        width={this.props.width}
        height={this.props.height}
      >
        {circles}
      </svg>
    );
  }
}

ReactDOM.render(
  <TaskGraphVisualiser graph={td as TaskGraph} width={800} height={600} />,
  document.getElementById("AppContainer")
);
