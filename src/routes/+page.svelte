<script lang="ts">
  type Vec2 = {
    x: number,
    y: number,
  };

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
    | Vec2 = "up"; // moving camera by such vector
  let cameraShift: Vec2 = {x: 0, y: 0};

  // TODO: coords in [-1, -1] x [1, 1]
  // let vertices: Vec2[] = [
  //   {x: 10, y: 10},
  //   {x: 100, y: 200},
  // ];
  // let edges: {
  //   // vertices indexes
  //   from: number,
  //   to: number,
  // }[] = [
  //   {from: 0, to: 1},
  // ];
  let {vertices, edges} = generateGraph(10);
  let intersections: {
    // edges indexes
    first: number,
    second: number,
    pt: Vec2,
  }[] = [];

  function minus(a: Vec2, b: Vec2): Vec2 {
    return {x: a.x - b.x, y: a.y - b.y}
  }

  function plus(a: Vec2, b: Vec2): Vec2 {
    return {x: a.x + b.x, y: a.y + b.y}
  }

  function multiply(v: Vec2, c: number): Vec2 {
    return {x: v.x * c, y: v.y * c};
  }

  function cross(a: Vec2, b: Vec2): number {
    return a.x * b.y - a.y * b.x;
  }

  type tmp = {
    from: Vec2,
    to: Vec2,
  };
  function intersect(v: tmp, w: tmp): Vec2 | null {
    const v1 = v.from, v2 = v.to;
    const w1 = w.from, w2 = w.to;
    const dw = minus(w2, w1);
    const dv = minus(v2, v1);
    const dvw1 = minus(v1, w1);
    const denom = cross(dv, dw);
    if (denom == 0) {
      return null;
    }
    const ua = cross(dw, dvw1) / denom;
    const ub = cross(dv, dvw1) / denom;
    if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
      return null;
    }
    return plus(v1, multiply(dv, ua));
  }

  function generateVertices(n: number) {
    let vertices: Vec2[] = [];
    for (let i = 0; i < n; i++) {
      vertices.push({x: Math.random() * 1000, y: Math.random() * 800})
    }
    return vertices;
  }

  function generateGraph(n: number) {
    const vertices = generateVertices(n);
    let allEdges: {
      i: number,
      j: number,
      from: Vec2,
      to: Vec2,
    }[] = [];
    for (let i = 0; i < vertices.length; i++) {
      for (let j = 0; j < i; j++) {
        allEdges.push({
          i,
          j,
          from: vertices[i],
          to: vertices[j],
        })
      }
    }
    for (let i = allEdges.length - 1; i > 0; i--) {
      let j = Math.floor(Math.random() * (i + 1));
      let temp = allEdges[i];
      allEdges[i] = allEdges[j];
      allEdges[j] = temp;
    }
    let edges = [allEdges[0]];
    for (let edge of allEdges) {
      let intersects = false;
      for (let bedge of edges) {
        if (intersect(edge, bedge)) {
          intersects = true;
          break;
        }
      }
      if (!intersects) {
        edges.push(edge);
      }
    }
    return {
      vertices: generateVertices(n),
      edges: edges.map(({i, j}) => {return {
        from: i,
        to: j,
      }}),
    };
  }
</script>

<div
  bind:clientWidth={width}
  bind:clientHeight={height}
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
    <polyline
      fill="none"
      stroke="red"
      stroke-width={graphicsConfig.edgeWidth}
      points={
        edges
          .map((e) => [vertices[e.from], vertices[e.to]])
          .map(([from, to]) => `${from.x},${from.y} ${to.x},${to.y}`)
          .join(" ")
      }
    />
    {#each vertices as vertex}
    	{vertex}
    {/each}
    {#each intersections as intersection}
    	{intersection}
    {/each}
    <text
      fill="white"
      dominant-baseline="central"
      text-anchor="middle"
      transform={`translate(${width/2}, ${20})`}
    >
      {intersections.length}
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