import React from "react";
import ReactDOM from "react-dom";

interface Props {
  content: string;
  width: number;
  lineHeight: number;
  textStyle: React.CSSProperties;
  x: number;
  y: number;
}

function wrapText(text: string, maxWidth: number, lineHeight: number): React.ReactElement[] {
  // TODO
  return [<tspan key={0}>{ text }</tspan>];
}

export const SVGTextBox: React.FunctionComponent<Props> = (props: Props) => {
  return (
    <text style={ props.textStyle } x={ props.x } y={ props.y } dominantBaseline="middle" textAnchor="middle">
      { wrapText(props.content, props.width, props.lineHeight) }
    </text>
  );
}
