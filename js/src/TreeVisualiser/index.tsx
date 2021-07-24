import React from "react";
import { tree, hierarchy, HierarchyPointNode } from "d3-hierarchy";
import { linkHorizontal } from "d3-shape";
import { TaskTree } from "../API/index";
import InteractiveSVG, { Rect } from "../InteractiveSVG";

interface Props {
  tree: TaskTree;
  minWidth: number;
  minHeight: number;
}
type State = Record<string, never>;

export class TreeVisualiser extends React.Component<Props, State> {
  layout: HierarchyPointNode<TaskTree>;

  constructor(props: Props) {
    super(props);
    const h = hierarchy(props.tree);
    this.layout = tree<TaskTree>().nodeSize([ 10, props.minWidth / (h.height + 1) ])(h);
  }

  render(): React.ReactNode {
    const bounds: Rect = {
      originX: 0,
      originY: 0,
      width: this.props.minWidth,
      height: this.props.minHeight,
    };

    const nodes = this.layout.descendants().map(n => {
      return <g key={n.data.id} strokeLinejoin="round" strokeWidth={3} transform={`translate(${n.y},${n.x})`}>
        <text dy="0.31em" fontSize="10px">{ n.data.name }</text>
      </g>;
    });

    const links = this.layout.links().map((l, i) => {
      const linkData: { source: [number, number], target: [number, number] } = {
        source: [l.source.y, l.source.x],
        target: [l.target.y, l.target.x],
      };
      return <g key={i} fill="none" stroke="#555" strokeOpacity={0.4} strokeWidth={1.5}>
        <path d={ linkHorizontal()(linkData) } />
      </g>;
    });

    return (
      <InteractiveSVG className="TreeVisualiser" bounds={ bounds } padding={ 50 }>
        { links }
        { nodes }
      </InteractiveSVG>
    );
  }
}
