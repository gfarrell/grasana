import React from "react";
import ReactDOM from "react-dom";
import { TreeVisualiser } from "./TreeVisualiser";
import { fetchProjectTree } from "./API";

async function main() {
  const data = await fetchProjectTree("XXX");
  ReactDOM.render(
    <TreeVisualiser tree={data} minWidth={800} minHeight={600} />,
    document.getElementById("AppContainer")
  );
}

main();
