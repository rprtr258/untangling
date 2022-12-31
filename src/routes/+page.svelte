<script lang="ts">
  import {onMount} from "svelte";
  import {minus, plus, multiply, cross, distSq} from "./Vec2";
  import type {Vec2} from "./Vec2";

  const graphicsConfig = {
    vertexRadius: 10,
    vertexColor: "#babdb6",
    edgeWidth: 3,
    intersectionRadius: 2,
    intersectionColor: "#cc0000",
    heldEdgeColor: "#505060",
    notHeldEdgeColor: "#000000",
    textColor: "white",
    backgroundColor: "#2e3436",
  };

  let screenSize: Vec2 = {x: 1200, y: 700};
  let mouseState:
    {type: "up"} // button is up
    | {type: "vertex", index: number} // holding vertex by that index
    | {type: "select", begin: Vec2, end: Vec2} // group selection
    | {type: "camera"}
    = {type: "up"}; // moving camera by such vector

  let zoom = 0;
  // camera position in screen coords
  let cameraPt: Vec2 = {x: 0, y: 0};

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

  const EPS = 1e-6;
  function intersect(v1: Vec2, v2: Vec2, w1: Vec2, w2: Vec2): Vec2 | null {
    const w = minus(w2, w1);
    const v = minus(v2, v1);
    const m = minus(v1, w1);
    const delta = cross(v, w);
    if (delta == 0) { // collinear
      return null;
    }
    const deltaA = cross(v, m) / delta;
    const deltaB = cross(w, m) / delta;
    if (deltaB <= EPS || deltaB >= 1-EPS || deltaA <= EPS || deltaA >= 1-EPS) {
      return null;
    }
    return plus(v1, multiply(v, deltaB));
  }

  function generateVertices(n: number) {
    let vertices: Vec2[] = [];
    for (let i = 0; i < n; i++) {
      vertices.push({x: Math.random(), y: Math.random()})
    }
    return vertices;
  }

  function shuffle<T>(list: T[]): T[] {
    for (let i = list.length - 1; i > 0; i--) {
      let j = Math.floor(Math.random() * (i + 1));
      let temp = list[i];
      list[i] = list[j];
      list[j] = temp;
    }
    return list
  }

  function generateGraph(n: number) {
    const vertices = generateVertices(n);
    let allEdges: {
      from: number,
      to: number,
    }[] = [];
    for (let i = 0; i < vertices.length; i++) {
      for (let j = 0; j < i; j++) {
        allEdges.push({from: i, to: j});
      }
    }
    allEdges = shuffle(allEdges);
    let edges2: typeof allEdges = [];
    for (let edge of allEdges) {
      let addsIntersection = false;
      for (let bedge of edges2) {
        if (intersect(
          vertices[edge.from],
          vertices[edge.to],
          vertices[bedge.from],
          vertices[bedge.to],
        ) !== null) {
          addsIntersection = true;
          break;
        }
      }
      // TODO: fix filtering too much edges
      if (!addsIntersection) {
        edges2.push(edge);
      }
    }
    return {
      vertices: generateVertices(n),
      edges: edges2,
    };
  }

  function onMouseMove(e: MouseEvent) {
    const mouseFinPt: Vec2 = {x: e.clientX, y: e.clientY};
    const halfPt = multiply(screenSize, 1/2);
    const mouseAbsPt: Vec2 = plus(multiply(minus(mouseFinPt, halfPt), 1/zoomCoeff), halfPt);
    const mouseScreenPt: Vec2 = minus(mouseAbsPt, cameraPt);
    const mouseNormPt: Vec2 = {
      x: mouseScreenPt.x / screenSize.x,
      y: mouseScreenPt.y / screenSize.y,
    };
    switch (mouseState.type) {
    case "vertex":
      g.vertices[mouseState.index] = mouseNormPt;
      break;
    case "camera":
      let moveFinPt: Vec2 = {x: e.movementX, y: e.movementY};
      const moveAbsPt: Vec2 = multiply(moveFinPt, 1/zoomCoeff);
      cameraPt = plus(cameraPt, moveAbsPt);
      break;
    case "select":
      mouseState.end = mouseNormPt;
      mouseState = mouseState;
      break;
    }
  }

  function onMouseDown(e: MouseEvent) {
    e.preventDefault();
    const mousePos: Vec2 = {x: e.clientX, y: e.clientY};
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
      const halfPt = multiply(screenSize, 1/2);
      const mouseAbsPt: Vec2 = plus(multiply(minus(mousePos, halfPt), 1/zoomCoeff), halfPt);
      const mouseScreenPt: Vec2 = minus(mouseAbsPt, cameraPt);
      const mouseNormPt: Vec2 = {
        x: mouseScreenPt.x / screenSize.x,
        y: mouseScreenPt.y / screenSize.y,
      };
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
    zoom -= e.deltaY;
  }

  function onMouseUp(_: MouseEvent) {
    mouseState = {type: "up"};
  }

  function normToFin(pt: Vec2, cameraPt: Vec2, zoomCoeff: number): Vec2 {
    const screenPt = {
      x: pt.x * screenSize.x,
      y: pt.y * screenSize.y,
    };
    const absPt = plus(screenPt, cameraPt);
    const halfPt = multiply(screenSize, 1/2);
    const finPt = plus(multiply(minus(absPt, halfPt), zoomCoeff), halfPt);
    return finPt;
  }

  $: realSelect = (() => {
    if (mouseState.type !== "select") {
      return null;
    }

    return {
      begin: normToFin(mouseState.begin, cameraPt, zoomCoeff),
      end: normToFin(mouseState.end, cameraPt, zoomCoeff),
    };
  })();

  $: zoomCoeff = Math.exp(zoom / 1000);

  $: realVertices = g.vertices.map((v) => normToFin(v, cameraPt, zoomCoeff));

  $: intersections = (() => {
    let newIntersections = [];
    for (let i = 0; i < g.edges.length; i++) {
      for (let j = 0; j < i; j++) {
        if (i == j) continue;
        const edge1 = g.edges[i];
        const edge2 = g.edges[j];
        const intersection = intersect(
          realVertices[edge1.from],
          realVertices[edge1.to],
          realVertices[edge2.from],
          realVertices[edge2.to],
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

  onMount(() => {
    g = generateGraph(10);
  });
</script>

<div
  bind:clientWidth={screenSize.x}
  bind:clientHeight={screenSize.y}
  on:mousemove={onMouseMove}
  on:mousedown={onMouseDown}
  on:mouseup={onMouseUp}
  on:contextmenu={(e)=>e.preventDefault()}
>
  <svg
    width="100%"
    height="100%"
    on:wheel={onWheel}
    >
    <rect
      width="100%"
      height="100%"
      fill={graphicsConfig.backgroundColor}
    />
    {#each g.edges as {from, to}}
      <polyline
        fill="none"
        stroke="black"
        stroke-width={graphicsConfig.edgeWidth}
        points={`${realVertices[from].x},${realVertices[from].y} ${realVertices[to].x},${realVertices[to].y}`}
      />
    {/each}
    {#each realVertices as {x, y}}
      <circle
        r={graphicsConfig.vertexRadius}
        fill={graphicsConfig.vertexColor}
        transform={`translate(${x},${y})`}
      />
    {/each}
    {#each intersections as {pt}}
    	<circle
        r={graphicsConfig.intersectionRadius}
        fill={graphicsConfig.intersectionColor}
        transform={`translate(${pt.x},${pt.y})`}
      />
    {/each}
    {#if realSelect !== null}
      <rect
        width="{Math.abs(realSelect.end.x-realSelect.begin.x)}px"
        height="{Math.abs(realSelect.end.y-realSelect.begin.y)}px"
        x={Math.min(realSelect.begin.x, realSelect.end.x)}
        y={Math.min(realSelect.begin.y, realSelect.end.y)}
        fill="red"
      />
    {/if}
    <text
      fill="white"
      dominant-baseline="central"
      text-anchor="middle"
      transform={`translate(${screenSize.x/2}, ${20})`}
    >
      {#if intersections.length === 0}
        vahui
      {:else}
        {intersections.length}
      {/if}
      ,{JSON.stringify(mouseState)}
    </text>
  </svg>
</div>

<style>
  :global(body) {
    margin: 0;
    height: 100%;
  }
  :global(div, html) {
    height: 100%;
  }
</style>