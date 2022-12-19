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
  let mouseState: "up" | number | Vec2 = "up";
  let cameraShift: Vec2 = {x: 0, y: 0};

  // TODO: coords in [-1, -1] x [1, 1]
  let vertices: Vec2[] = [
    {x: 10, y: 10},
    {x: 100, y: 200},
  ];
  let edges: {
    // vertices indexes
    from: number,
    to: number,
  }[] = [
    {from: 0, to: 1},
  ];
  let intersections: {
    // edges indexes
    first: number,
    second: number,
    pt: Vec2,
  }[] = [];
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
      stroke="red"
      stroke-width=10
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