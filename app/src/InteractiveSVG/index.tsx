import React from "react";

export interface Rect {
  originX: number;
  originY: number;
  width: number;
  height: number;
}

interface Props {
  className?: string;
  id?: string;
  children?: React.ReactNode[];
  bounds: Rect;
  padding?: number;
}

interface State {
  panning: boolean;
  panX: number;
  panY: number;
  zoom: number;
}

export default class InteractiveSVG extends React.PureComponent<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      panX: 0,
      panY: 0,
      zoom: 1,
      panning: false,
    };
  }

  pan(x: number, y: number): void {
    const { zoom, panX, panY } = this.state;
    this.setView(zoom, x + panX, y + panY);
  }

  zoom(amount: number): void {
    const { zoom, panX, panY } = this.state;
    this.setView(zoom + amount, panX, panY);
  }

  setView(zoom: number, panX: number, panY: number): void {
    this.setState({ zoom: zoom <= 0 ? this.state.zoom : zoom, panX, panY });
  }

  getViewRect(): Rect {
    const { bounds, padding } = this.props;
    const { zoom, panX, panY } = this.state;
    return {
      originX: bounds.originX + panX,
      originY: bounds.originY + panY,
      width: (bounds.width + padding || 0) / zoom,
      height: (bounds.height + padding || 0) / zoom,
    };
  }

  render(): React.ReactNode {
    const rect = this.getViewRect();
    const startDrag = () => this.setState({ panning: true });
    const stopDrag = () => this.setState({ panning: false });
    const handleDrag = (e: React.MouseEvent): void => {
      if(!this.state.panning) return;
      const { movementX, movementY } = e;
      this.pan(-movementX, -movementY);
    };
    const handleZoom = (e: React.WheelEvent): void => {
      this.zoom(e.deltaY / 500);
    }
    return (
      <svg
        id={this.props.id}
        className={this.props.className}
        viewBox={ `${rect.originX} ${rect.originY} ${rect.width} ${rect.height}` }
        onMouseDown={ startDrag }
        onMouseUp={ stopDrag }
        onMouseLeave={ stopDrag }
        onMouseMove={ handleDrag }
        onWheel={ handleZoom }
      >{ this.props.children }</svg>
    );
  }
}
