<script lang="ts">
  import {onMount} from "svelte";
  import {minus, plus, multiply, cross, distSq, scaleXY, unembed, apply, embed} from "./math";
  import type {Vec2, Vec3, Mat3} from "./math";

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

  let screenSize: Vec2 = [1200, 700];
  let mouseState:
    {type: "up"} // button is up
    | {type: "vertex", index: number} // holding vertex by that index
    | {type: "select", begin: Vec2, end: Vec2} // group selection
    | {type: "camera"} // moving camera by such vector
    = {type: "up"};

  let camera: {
    zoom: number,
    // camera position in screen coords
    shift: Vec2,
  } = {
    zoom: 0,
    shift: [0,0],
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
      vertices.push([Math.random(), Math.random()]);
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
    for (let edge1 of allEdges) {
      let addsIntersection = false;
      for (let edge2 of edges2) {
        if (intersect(
          vertices[edge1.from],
          vertices[edge1.to],
          vertices[edge2.from],
          vertices[edge2.to],
        ) !== null) {
          addsIntersection = true;
          break;
        }
      }
      // TODO: fix filtering too much edges
      if (!addsIntersection) {
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
    const halfPt = multiply(screenSize, 1/2);
    const mouseAbsPt: Vec2 = plus(
      multiply(minus(mouseFinPt, halfPt), 1/zoomCoeff),
      halfPt,
    );
    const mouseScreenPt: Vec2 = minus(mouseAbsPt, camera.shift);
    const mouseNormPt: Vec2 = [
      mouseScreenPt[0] / screenSize[0],
      mouseScreenPt[1] / screenSize[1],
    ];
    switch (mouseState.type) {
    case "vertex":
      if (!selectedVertices.includes(mouseState.index)) {
        g.vertices[mouseState.index] = mouseNormPt;
      } else {
        const move = minus(mouseNormPt, g.vertices[mouseState.index]);
        for (const vertexIdx of selectedVertices) {
          g.vertices[vertexIdx] = plus(g.vertices[vertexIdx], move);
        }
      }
      break;
    case "camera":
      let moveFinPt: Vec2 = [e.movementX, e.movementY];
      const moveAbsPt: Vec2 = multiply(moveFinPt, 1/zoomCoeff);
      camera.shift = plus(camera.shift, moveAbsPt);
      break;
    case "select":
      mouseState.end = mouseNormPt;
      mouseState = mouseState;
      selectedVertices = ((): number[] => {
        if (mouseState.type !== "select") {
          return [];
        }

        const minX = Math.min(mouseState.begin[0], mouseState.end[0]);
        const maxX = Math.max(mouseState.begin[0], mouseState.end[0]);
        const minY = Math.min(mouseState.begin[1], mouseState.end[1]);
        const maxY = Math.max(mouseState.begin[1], mouseState.end[1]);

        let res = [];
        for (let i = 0; i < g.vertices.length; i++) {
          const v = g.vertices[i];
          if (v[0] < minX || v[0] > maxX || v[1] < minY || v[1] > maxY) {
            continue;
          }
          res.push(i);
        }
        return res;
      })();
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
      const halfPt = multiply(screenSize, 1/2);
      const mouseAbsPt: Vec2 = plus(
        multiply(minus(mousePos, halfPt), 1/zoomCoeff),
        halfPt,
      );
      const mouseScreenPt: Vec2 = minus(mouseAbsPt, camera.shift);
      const mouseNormPt: Vec2 = [
        mouseScreenPt[0] / screenSize[0],
        mouseScreenPt[1] / screenSize[1],
      ];
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
    camera.zoom -= e.deltaY;
  }

  function onMouseUp(_: MouseEvent) {
    mouseState = {type: "up"};
  }

  function normToFin(pt: Vec2, cameraPt: Vec2, zoomCoeff: number): Vec2 {
    const screenPt: Vec2 = unembed(apply(scaleXY(screenSize), embed(pt)));
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
      begin: normToFin(mouseState.begin, camera.shift, zoomCoeff),
      end: normToFin(mouseState.end, camera.shift, zoomCoeff),
    };
  })();

  $: zoomCoeff = Math.exp(camera.zoom / 1000);

  $: realVertices = g.vertices.map((v) => {
    return normToFin(v, camera.shift, zoomCoeff);
  });

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
    // camera = {
    //   zoom: 0,
    //   shift: [screenSize.width / 2, screenSize.height / 2],
    // };
  });
</script>

<div
  bind:clientWidth={screenSize[0]}
  bind:clientHeight={screenSize[1]}
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
        points={`${realVertices[from][0]},${realVertices[from][1]} ${realVertices[to][0]},${realVertices[to][1]}`}
      />
    {/each}
    {#each realVertices as v, i}
      <circle
        r={graphicsConfig.vertexRadius}
        fill={selectedVertices.includes(i) ? "#fa5b56" : graphicsConfig.vertexColor}
        transform={`translate(${v[0]},${v[1]})`}
      />
    {/each}
    {#each intersections as {pt}}
    	<circle
        r={graphicsConfig.intersectionRadius}
        fill={graphicsConfig.intersectionColor}
        transform={`translate(${pt[0]},${pt[1]})`}
      />
    {/each}
    {#if realSelect !== null}
      <rect
        id="select"
        width="{Math.abs(realSelect.end[0]-realSelect.begin[0])}px"
        height="{Math.abs(realSelect.end[1]-realSelect.begin[1])}px"
        x={Math.min(realSelect.begin[0], realSelect.end[0])}
        y={Math.min(realSelect.begin[1], realSelect.end[1])}
      />
    {/if}
    <text
      fill="white"
      dominant-baseline="central"
      text-anchor="middle"
      transform={`translate(${screenSize[0]/2}, ${20})`}
    >
      {#if intersections.length === 0}
        vahui
      {:else}
        {intersections.length}
      {/if}
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
  #select {
    fill: none;
    stroke-width: .2rem;
    stroke: #ff1612;
    stroke-dasharray: .3rem .4rem 1rem .4rem;
  }
</style>
