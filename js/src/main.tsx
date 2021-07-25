import React from "react";
import ReactDOM from "react-dom";
import { TaskTree } from "./API";
import { TreeVisualiser } from "./TreeVisualiser";

declare global {
  interface Window {
    treejson: TaskTree;
  }
}

async function main() {
  ReactDOM.render(
    <TreeVisualiser tree={window.treejson} minWidth={800} minHeight={600} />,
    document.getElementById("AppContainer")
  );
}

main();
