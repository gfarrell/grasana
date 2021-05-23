import React from "react";
import ReactDOM from "react-dom";
import { ForceGraphVisualiser } from "./ForceGraphVisualiser";
import { fetchProjectGraph } from "./API";

async function main() {
  const data = await fetchProjectGraph("XXX");
  ReactDOM.render(
    <ForceGraphVisualiser graph={data} minWidth={800} minHeight={600} />,
    document.getElementById("AppContainer")
  );
}

main();
