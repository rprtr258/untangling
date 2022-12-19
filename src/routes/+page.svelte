<script lang="ts">
  import {onMount} from "svelte";
  import {minus, plus, multiply, cross, dot} from "./Vec2";
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

  let width;
  let height;
  let mouseState: "up" // button is up
    | number // holding vertex by that index
    | "camera" = "up"; // moving camera by such vector
  let cameraShift: Vec2 = {x: 0, y: 0};

  // TODO: coords in [-1, -1] x [1, 1]
  let vertices: Vec2[] = [];
  let edges: {
    from: number,
    to: number,
  }[] = [];
  let intersections: {
    // edges indexes
    first: number,
    second: number,
    pt: Vec2,
  }[] = [];

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
      vertices.push({x: Math.random() * 1200, y: Math.random() * 700})
    }
    return vertices;
  }

  function generateGraph(n: number) {
    const vertices = generateVertices(n);
    let allEdges: {
      i: number,
      j: number,
    }[] = [];
    for (let i = 0; i < vertices.length; i++) {
      for (let j = 0; j < i; j++) {
        allEdges.push({i, j});
      }
    }
    for (let i = allEdges.length - 1; i > 0; i--) {
      let j = Math.floor(Math.random() * (i + 1));
      let temp = allEdges[i];
      allEdges[i] = allEdges[j];
      allEdges[j] = temp;
    }
    let edges2: typeof allEdges = [];
    for (let edge of allEdges) {
      let arolf = false;
      for (let bedge of edges2) {
        if (intersect(
          vertices[edge.i],
          vertices[edge.j],
          vertices[bedge.i],
          vertices[bedge.j],
        ) !== null) {
          arolf = true;
          break;
        }
      }
      if (!arolf) {
        edges2.push(edge);
      }
    }
    return {
      vertices: generateVertices(n),
      edges: edges2.map(({i, j}) => {return {
        from: i,
        to: j,
      }}),
    };
  }

  function onMouseMove(e: MouseEvent) {
    if (typeof mouseState === "number") {
      vertices[mouseState] = minus({x: e.clientX, y: e.clientY}, cameraShift);
    } else if (mouseState === "camera") {
      cameraShift = plus(cameraShift, {x: e.movementX, y: e.movementY});
    }
  }

  function onMouseDown(e: MouseEvent) {
    for (let i = 0; i < vertices.length; i++) {
      const vertex = realVertices[i];
      const radii = minus(
        {x: e.clientX, y: e.clientY},
        vertex,
      );
      // TODO: find closest
      if (dot(radii, radii) <= graphicsConfig.vertexRadius ** 2) {
        mouseState = i;
        return;
      }
    }
    mouseState = "camera";
  }

  function onMouseUp(e: MouseEvent) {
    mouseState = "up";
  }

  $: realVertices = vertices.map((v) => plus(v, cameraShift));
  $: intersections = (() => {
    let newIntersections = [];
    for (let i = 0; i < edges.length; i++) {
      for (let j = 0; j < i; j++) {
        if (i == j) continue;
        const edge1 = edges[i];
        const edge2 = edges[j];
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
    const xdd = generateGraph(10);
    vertices = xdd.vertices;
    edges = xdd.edges;
  });
</script>

<div
  bind:clientWidth={width}
  bind:clientHeight={height}
  on:mousemove={onMouseMove}
  on:mousedown={onMouseDown}
  on:mouseup={onMouseUp}
>
  <svg
    width="100%"
    height="100%"
  >
    <rect
      width="100%"
      height="100%"
      fill={graphicsConfig.backgroundColor}
    />
    {#each edges as {from, to}}
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
    <text
      fill="white"
      dominant-baseline="central"
      text-anchor="middle"
      transform={`translate(${width/2}, ${20})`}
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
</style>