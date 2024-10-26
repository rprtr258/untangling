// @ts-check
/** @typedef {import('./math.js').Vec2} Vec2 */
/** @typedef {import('./math.js').Vec3} Vec3 */
/** @typedef {import('./math.js').Mat3} Mat3 */

import {
  multiply, plus, minus, distSq,
  scaleXY, translate, scale,
  compose, invert, intersect, apply2, minmax,
  eye,
} from "./math.js";
import { filterMap, generate, shuffle } from "./list.js";

/**
 * @param {number} n
 */
function generateVertices(n) {
  return generate(
    n,
    /**
     * @returns {Vec2}
     */
    () => [Math.random(), Math.random()]);
}

/**
 * @param {number} n
 * @returns
 */
function generateGraph(n) {
  const vertices = generateVertices(n);
  const allEdges = shuffle(generate(
    vertices.length,
    i => generate(
      i,
      j => { return { from: i, to: j } },
    ),
  ).flat());
  /** @type {typeof allEdges} */
  let edges2 = [];
  for (let edge1 of allEdges) {
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

export default {
  data() {
    /** @type {Vec2} */
    const screenSize = [1920, 1080];
    return {
      screenSize: screenSize,
      graphicsConfig: {
        vertexRadius: 10,
        vertexColor: "#babdb6",
        edgeWidth: 3,
        intersectionRadius: 2,
        intersectionColor: "#cc0000",
        heldEdgeColor: "#505060",
        notHeldEdgeColor: "#000000",
        textColor: "#ffffff",
        backgroundColor: "#2e3436",
      },
      /** @type {{type: "up"} // button is up
       *    | {type: "vertex", index: number} // holding vertex by that index
       *    | {type: "select", begin: Vec2, end: Vec2} // group selection
       *    | {type: "camera"} // moving camera by such vector
       * }
      */
      mouseState: { type: "up" },
      /** @type {Mat3} */
      camera: scaleXY(screenSize),
      /** @type {{
        vertices: Vec2[],
        // coords in [0, 1] x [0, 1]
        edges: {
          from: number,
          to: number,
        }[]
      }} */
      g: generateGraph(10),
      /** @type {number[]} */
      selectedVertices: [],
    }
  },
  methods: {
    /**
     * @param {MouseEvent} e
     */
    onMouseMove(e) {
      /** @type {Vec2} */
      const mouseFinPt = [e.clientX, e.clientY];
      const mouseNormPt = apply2(invert(this.camera), mouseFinPt);
      switch (this.mouseState.type) {
        case "vertex":
          if (!this.selectedVertices.includes(this.mouseState.index)) {
            this.g.vertices[this.mouseState.index] = mouseNormPt;
          } else {
            const diff = minus(mouseNormPt, this.g.vertices[this.mouseState.index]);
            const move = translate(diff);
            for (const vertexIdx of this.selectedVertices) {
              this.g.vertices[vertexIdx] = apply2(move, this.g.vertices[vertexIdx]);
            }
          }
          break;
        case "camera":
          /** @type {Vec2} */
          let moveFinPt = [e.movementX, e.movementY];
          this.camera = compose(this.camera, translate(moveFinPt));
          break;
        case "select":
          this.mouseState.end = mouseNormPt;
          const [minX, maxX] = minmax(this.mouseState.begin[0], this.mouseState.end[0]);
          const [minY, maxY] = minmax(this.mouseState.begin[1], this.mouseState.end[1]);
          this.selectedVertices = filterMap(
            this.g.vertices,
            (v, i) => [
              i,
              v[0] >= minX && v[0] <= maxX &&
              v[1] >= minY && v[1] <= maxY,
            ],
          );
          break;
      }
    },
    /**
     * @param {MouseEvent} e
     */
    onMouseDown(e) {
      e.preventDefault();
      /** @type {Vec2} */
      const mousePos = [e.clientX, e.clientY];
      if (e.button == 0) { // LMB
        for (let i = 0; i < this.g.vertices.length; i++) {
          const vertex = this.realVertices[i];
          const radii = minus(mousePos, vertex);
          // TODO: find closest
          if (distSq(radii) <= this.graphicsConfig.vertexRadius ** 2) {
            this.mouseState = { type: "vertex", index: i };
            console.log("clicked vertex", i);
            return;
          }
        }
        this.mouseState = { type: "camera" };
      } else if (e.button == 2) { // RMB
        const mouseNormPt = apply2(invert(this.camera), mousePos);
        this.mouseState = {
          type: "select",
          begin: mouseNormPt,
          end: mouseNormPt,
        };
      }
    },
    /**
     * @param {WheelEvent & {
     *   currentTarget: EventTarget & SVGSVGElement,
     * }} e
    */
    onWheel(e) {
      e.preventDefault();
      const alpha = Math.exp(-e.deltaY / 1000);
      /** @type {Vec2} */
      const mouseFinPt = [e.clientX, e.clientY];
      this.camera = compose(
        this.camera,
        invert(translate(mouseFinPt)),
        scale(alpha),
        translate(mouseFinPt),
      );
    },
    /** @param {MouseEvent} _ */
    onMouseUp(_) {
      this.mouseState = { type: "up" };
    },
  },
  computed: {
    realSelect() {
      const state = this.mouseState;
      if (state.type !== "select") {
        return null;
      }

      return {
        begin: apply2(this.camera, state.begin),
        end: apply2(this.camera, state.end),
      };
    },
    realVertices() {
      return this.g.vertices.map((/** @type {Vec2} */ v) => apply2(this.camera, v));
    },
    intersections() {
      let newIntersections = [];
      for (let i = 0; i < this.g.edges.length; i++) {
        for (let j = 0; j < i; j++) {
          const edge1 = this.g.edges[i];
          const edge2 = this.g.edges[j];
          const intersection = intersect(
            [this.realVertices[edge1.from], this.realVertices[edge1.to]],
            [this.realVertices[edge2.from], this.realVertices[edge2.to]],
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
    },
  },
  template: /*html*/`
    <div
      @mousemove="onMouseMove"
      @mousedown="onMouseDown"
      @mouseup="onMouseUp"
      @contextmenu="(e)=>e.preventDefault()"
      style="height: 100vh; width: 100wh;"
    >
      <svg
        width="100%"
        height="100%"
        @wheel="onWheel"
      >
        <rect
          width="100%"
          height="100%"
          :fill="graphicsConfig.backgroundColor"
        />
        <polyline v-for="{from, to} in g.edges"
          fill="none"
          stroke="black"
          :stroke-width="graphicsConfig.edgeWidth"
          :points="realVertices[from][0]+','+realVertices[from][1]+' '+realVertices[to][0]+','+realVertices[to][1]"
        />
        <circle v-for="[i, v] in Object.entries(realVertices)"
          :r="graphicsConfig.vertexRadius"
          :fill="selectedVertices.includes(Number(i)) ? '#fa5b56' : graphicsConfig.vertexColor"
          :transform="'translate('+v[0]+','+v[1]+')'"
        />
        <circle v-for="{pt} in intersections"
          :r="graphicsConfig.intersectionRadius"
          :fill="graphicsConfig.intersectionColor"
          :transform="'translate('+pt[0]+','+pt[1]+')'"
        />
        <rect v-if="realSelect !== null"
          id="select"
          :width="Math.abs(realSelect.end[0]-realSelect.begin[0])+'px'"
          :height="Math.abs(realSelect.end[1]-realSelect.begin[1])+'px'"
          :x="Math.min(realSelect.begin[0], realSelect.end[0])"
          :y="Math.min(realSelect.begin[1], realSelect.end[1])"
        />
        <text
          fill="white"
          dominant-baseline="central"
          text-anchor="middle"
          :transform="'translate('+screenSize[0]/2+', 20)'"
        >
          {{intersections.length === 0 ? "vahui" : "intersections: " + intersections.length}}
        </text>
      </svg>
    </div>
  `
}
