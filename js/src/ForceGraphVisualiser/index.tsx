import React from "react";
import { Task, EdgeType, Edge, TaskGraph } from "../API";
import {
  forceSimulation,
  Simulation,
  forceManyBody,
  forceLink,
  forceCenter,
  forceCollide,
  SimulationNodeDatum,
  SimulationLinkDatum,
} from "d3-force";
import { line, curveCardinal } from "d3";
import InteractiveSVG, { Rect } from "../InteractiveSVG";
import { SVGTextBox } from "../SVGTextBox";

interface GraphNode extends SimulationNodeDatum {
  id: string;
  name: string;
  x: number;
  y: number;
}
interface GraphEdge extends SimulationLinkDatum<GraphNode> {
  source: string|GraphNode;
  target: string|GraphNode;
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

interface Props {
  graph: TaskGraph;
  minWidth: number;
  minHeight: number;
}

interface State {
  nodes: GraphNode[];
  edges: GraphEdge[];
  minx: number;
  miny: number;
  maxx: number;
  maxy: number;
  zoom: number;
  panx: number;
  pany: number;
  panning: boolean;
}

const NODE_RADIUS = 5;

export class ForceGraphVisualiser extends React.Component<Props, State> {
  simulation: Simulation<GraphNode, GraphEdge>;

  constructor(props: Props) {
    super(props);

    const nodes = props.graph[0].map(taskToGraphNode);
    const edges = props.graph[1].map(edgeToGraphEdge);

    this.simulation = forceSimulation(nodes)
      .force(
        "charge",
        forceManyBody().strength(() => -10)
      )
      .force("collide", forceCollide(NODE_RADIUS * 4))
      .force(
        "link",
        forceLink(edges)
          .id((n: GraphNode) => n.id)
          .distance(() => 100)
      )
      .force("center", forceCenter(props.minWidth / 2, props.minHeight / 2));

    this.state = { nodes, edges, minx: 0, miny: 0, maxx: props.minWidth, maxy: props.minHeight, zoom: 1, panx: 0, pany: 0, panning: false };
    this.simulation.on("tick", () => this.updateNodePositions());
  }

  updateNodePositions(): void {
    const sFragment: Pick<State, "minx"|"miny"|"maxx"|"maxy"> = this.simulation.nodes().reduce((s, node) => {
      let { minx, miny, maxx, maxy } = s;

      if (node.x < minx && node.x < 0) minx = node.x;
      if (node.y < miny && node.y < 0) miny = node.y;
      if (node.x > maxx && node.x > 0) maxx = node.x;
      if (node.y > maxy && node.y > 0) maxy = node.y;
      return { minx, miny, maxx, maxy };
    }, this.state);
    this.setState(Object.assign(sFragment, {
      nodes: this.simulation.nodes(),
    }));
  }

  render(): React.ReactNode {
    const circles = this.state.nodes.map((n) => (
      <circle
        key={n.id}
        style={{ fill: "#000" }}
        stroke="#000"
        strokeWidth={0.5}
        r={NODE_RADIUS}
        cx={n.x}
        cy={n.y}
      />
    ));
    const lineGenerator = line().curve(curveCardinal);
    const lines = this.state.edges.map((e, i) => {
      const source = e.source as GraphNode; // d3 does some cray internal mutation crap
      const target = e.target as GraphNode; // d3 does some cray internal mutation crap
      return <path key={i} stroke="#AAA" strokeWidth={2} fill="none" d={ lineGenerator([[ source.x, source.y ], [ (source.x + target.x)/2, (source.y + target.y)/2], [target.x, target.y]])} />;
    });
    const textStyle: React.CSSProperties = {
      userSelect: "none",
      fontSize: 10,
      /* maxWidth: NODE_RADIUS * 2.5, */
    };
    const labels = this.state.nodes.map((n) => (
      <SVGTextBox textStyle={ textStyle } key={ n.id } x={ n.x } y={ n.y } width={ NODE_RADIUS * 2.5 } lineHeight={ 1.1 } content={ n.name } />
    ));
    const bounds: Rect = {
      originX: this.state.minx,
      originY: this.state.miny,
      width: this.state.maxx - this.state.minx,
      height: this.state.maxy - this.state.miny,
    };
    return (
      <InteractiveSVG className="TaskGraphVisualiser" bounds={ bounds } padding={ NODE_RADIUS }>
        {circles}
        {lines}
        {labels}
      </InteractiveSVG>
    );
  }
}
