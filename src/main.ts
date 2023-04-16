import {
  minus, distSq,
  scaleXY, translate, scale,
  unembed, apply, embed, compose, invert, intersect, poop as apply2, minmax, eye,
} from "./math";
import type {Vec2, Mat3} from "./math";
import {filterMap, generate, shuffle} from "./list";

const graphicsConfig = {
  vertexRadius: 10,
  vertexColor: "#babdb6",
  edgeWidth: 3,
  intersectionRadius: 2,
  intersectionColor: "#cc0000",
  heldEdgeColor: "#505060",
  notHeldEdgeColor: "#000000",
  textColor: "#ffffff",
  backgroundColor: "#2e3436",
};

let screenSize: Vec2 = [1200, 700];
let mouseState:
  {type: "up"} // button is up
  | {type: "vertex", index: number} // holding vertex by that index
  | {type: "select", begin: Vec2, end: Vec2} // group selection
  | {type: "camera"} // moving camera by such vector
  = {type: "up"};

type Camera = {
  zoom: Mat3,
  // camera position in screen coords
  shift: Mat3,
};
let camera: Camera = {
  zoom: scale(1),
  shift: eye,
  //shift: [screenSize.width / 2, screenSize.height / 2],
};

let g: {
  // coords in [0, 1] x [0, 1]
  vertices: Vec2[],
  edges: {
    from: number,
    to: number,
  }[],
} = {
  vertices: [],
  edges: [],
};
let selectedVertices: number[] = [];

function generateVertices(n: number) {
  return generate(n, (): Vec2 => [Math.random(), Math.random()]);
}

function generateGraph(n: number) {
  const vertices = generateVertices(n);
  const allEdges = shuffle(generate(
    vertices.length,
    i => generate(
      i,
      j => {return {from: i, to: j}},
    ),
  ).flat());
  let edges2: typeof allEdges = [];
  for (let edge1 of allEdges) {
    // TODO: fix filtering too much edges
    if (edges2.every((edge2) => intersect(
      [vertices[edge1.from], vertices[edge1.to]],
      [vertices[edge2.from], vertices[edge2.to]],
    ) === null)) {
      edges2.push(edge1);
    }
  }
  return {
    vertices: generateVertices(n),
    edges: edges2,
  };
}

function onMouseMove(e: MouseEvent) {
  const mouseFinPt: Vec2 = [e.clientX, e.clientY];
  const mouseNormPt = unembed(apply(
    invert(normToFin(camera)),
    embed(mouseFinPt),
  ));
  switch (mouseState.type) {
  case "vertex":
    if (!selectedVertices.includes(mouseState.index)) {
      g.vertices[mouseState.index] = mouseNormPt;
    } else {
      const diff = minus(mouseNormPt, g.vertices[mouseState.index]);
      const move = translate(diff);
      for (const vertexIdx of selectedVertices) {
        g.vertices[vertexIdx] = apply2(move, g.vertices[vertexIdx]);
      }
    }
    break;
  case "camera":
    let moveFinPt: Vec2 = [e.movementX, e.movementY];
    const moveAbsPt: Vec2 = unembed(apply(
      invert(camera.zoom),
      embed(moveFinPt),
    ));
    camera.shift = compose(
      translate(moveAbsPt),
      camera.shift,
    );
    break;
  case "select":
    mouseState = {...mouseState, end: mouseNormPt};
    const [minX, maxX] = minmax(mouseState.begin[0], mouseState.end[0]);
    const [minY, maxY] = minmax(mouseState.begin[1], mouseState.end[1]);
    selectedVertices = filterMap(
      g.vertices,
      (v, i) => [
        i,
        v[0] >= minX && v[0] <= maxX &&
        v[1] >= minY && v[1] <= maxY,
      ],
    );
    break;
  }
}

function onMouseDown(e: MouseEvent) {
  e.preventDefault();
  const mousePos: Vec2 = [e.clientX, e.clientY];
  if (e.button == 0) { // LMB
    for (let i = 0; i < g.vertices.length; i++) {
      const vertex = realVertices[i];
      const radii = minus(mousePos, vertex);
      // TODO: find closest
      if (distSq(radii) <= graphicsConfig.vertexRadius ** 2) {
        mouseState = {type: "vertex", index: i};
        return;
      }
    }
    mouseState = {type: "camera"};
  } else if (e.button == 2) { // RMB
    const mouseNormPt = unembed(apply(
      invert(normToFin(camera)),
      embed(mousePos),
    ));
    mouseState = {
      type: "select",
      begin: mouseNormPt,
      end: mouseNormPt,
    };
  }
}

function onWheel(e: WheelEvent & {
  currentTarget: EventTarget & SVGSVGElement,
}) {
  e.preventDefault();
  camera.zoom = compose(
    scale(Math.exp(-e.deltaY / 1000)),
    camera.zoom,
  );
}

function onMouseUp(_: MouseEvent) {
  mouseState = {type: "up"};
}

function normToFin(camera: Camera): Mat3 {
  const halfPtTranslate = translate([
    screenSize[0] / 2,
    screenSize[1] / 2,
  ]);
  return compose(
    scaleXY(screenSize),
    camera.shift,
    invert(halfPtTranslate),
    camera.zoom,
    halfPtTranslate,
  );
}

let realSelect = (() => {
  // if (mouseState.type !== "select") {
    // return null;
  // }

  const m = normToFin(camera);
  return {
    begin: [],// apply2(m, mouseState.begin),
    end:   [],// apply2(m, mouseState.end),
  };
})();

let realVertices = (() => {
  const transform = normToFin(camera);
  return g.vertices.map((v) => apply2(transform, v));
})();

let intersections = (() => {
  let newIntersections = [];
  for (let i = 0; i < g.edges.length; i++) {
    for (let j = 0; j < i; j++) {
      const edge1 = g.edges[i];
      const edge2 = g.edges[j];
      const intersection = intersect(
        [realVertices[edge1.from], realVertices[edge1.to]],
        [realVertices[edge2.from], realVertices[edge2.to]],
      );
      if (intersection === null) {
        continue;
      }
      newIntersections.push({
        first: i,
        second: j,
        pt: intersection,
      });
    }
  }
  return newIntersections;
})();

g = generateGraph(10);
// camera = {
//   zoom: 0,
//   shift: [screenSize.width / 2, screenSize.height / 2],
// };

const body = document.getElementsByTagName("body")[0];


const rootDiv = document.createElement("div");
rootDiv.setAttribute("clientWidth", `${screenSize[0]}`); // TODO: bind
rootDiv.setAttribute("clientHeight", `${screenSize[1]}`); // TODO: bind
rootDiv.onmousemove = onMouseMove;
rootDiv.onmousedown = onMouseDown;
rootDiv.onmouseup = onMouseUp;
rootDiv.oncontextmenu = (e)=>e.preventDefault();

body.appendChild(rootDiv);


const rootSVG = document.createElement("svg") as any as SVGSVGElement;
rootSVG.setAttribute("width", "100%");
rootSVG.setAttribute("height", "100%");
// rootSVG.onwheel = onWheel; // TODO: fix type fuckup

rootDiv.appendChild(rootSVG);


const backgroundRect = document.createElement("rect");
backgroundRect.setAttribute("width", "100%");
backgroundRect.setAttribute("height", "100%");
backgroundRect.setAttribute("fill", graphicsConfig.backgroundColor); // TODO: bind

rootSVG.appendChild(backgroundRect);


// TODO: bind
for (const {from, to} of g.edges) {
  const edgeLine = document.createElement("polyline");
  edgeLine.setAttribute("fill", "none");
  edgeLine.setAttribute("stroke", "black");
  edgeLine.setAttribute("stroke-width", `${graphicsConfig.edgeWidth}`); // TODO: bind
  edgeLine.setAttribute("points", `${realVertices[from][0]},${realVertices[from][1]} ${realVertices[to][0]},${realVertices[to][1]}`); // TODO: bind

  rootSVG.appendChild(edgeLine);
}


// TODO: bind
for (let i = 0; i < realVertices.length; i++) {
  const v = realVertices[i];
  const vertexCircle = document.createElement("circle");
  vertexCircle.setAttribute("r", `${graphicsConfig.vertexRadius}`);
  vertexCircle.setAttribute("fill", selectedVertices.includes(i) ? "#fa5b56" : graphicsConfig.vertexColor);
  vertexCircle.setAttribute("transform", `translate(${v[0]},${v[1]})`); // TODO: bind

  rootSVG.appendChild(vertexCircle);
}


// TODO: bind
for (const {pt} of intersections) {
  const intersectionCircle = document.createElement("circle");
  intersectionCircle.setAttribute("r", graphicsConfig.intersectionRadius.toString());
  intersectionCircle.setAttribute("fill", graphicsConfig.intersectionColor);
  intersectionCircle.setAttribute("transform", `translate(${pt[0]},${pt[1]})`); // TODO: bind

  rootSVG.appendChild(intersectionCircle);
}


// TODO: bind
if (realSelect !== null) {
  const selectionRect = document.createElement("rect");
  selectionRect.setAttribute("id", "select");
  selectionRect.setAttribute("width", `${Math.abs(realSelect.end[0]-realSelect.begin[0])}px`);
  selectionRect.setAttribute("height", `${Math.abs(realSelect.end[1]-realSelect.begin[1])}px`);
  selectionRect.setAttribute("x", Math.min(realSelect.begin[0], realSelect.end[0]).toString());
  selectionRect.setAttribute("y", Math.min(realSelect.begin[1], realSelect.end[1]).toString()); // TODO: bind

  rootSVG.appendChild(selectionRect);
}


const scoreText = document.createElement("text");
scoreText.setAttribute("fill", "white");
scoreText.setAttribute("dominant-baseline", "central");
scoreText.setAttribute("text-anchor", "middle");
scoreText.setAttribute("transform", `translate(${screenSize[0]/2}, ${20})`);
scoreText.textContent = (intersections.length === 0) ? "vahui" : intersections.length.toString();

rootSVG.appendChild(scoreText);
