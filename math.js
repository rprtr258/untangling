// @ts-check

/** @typedef {[
  [number, number, number],
  [number, number, number],
  [number, number, number]
]} Mat3 */

export const eye = translate([0, 0]);

/** @typedef {[number, number]} Vec2 */
/** @typedef {[number, number, number]} Vec3 */

/**
 * @param {Vec2} v 
 * @returns {Vec3}
 */
export function embed(v) {
  return [v[0], v[1], 1];
}

/**
 * @param {Vec3} v 
 * @returns {Vec2}
 */
export function unembed(v) {
  return [x(v), y(v)];
}

/**
 * @param {Vec3} v 
 * @returns {number}
 */
export function x(v) {
  return v[0] / v[2];
}

/**
 * @param {Vec3} v 
 * @returns {number}
 */
export function y(v) {
  return v[1] / v[2];
}

/**
 * @param {...Mat3} ms
 * @returns {Mat3}
 */
export function compose(...ms) {
  /** @type {Mat3} */
  let res = [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1],
  ];
  for (let m of ms) {
    /** @type {Mat3} */
    let newRes = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ];
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 3; j++) {
        for (let k = 0; k < 3; k++) {
          newRes[i][j] += m[i][k] * res[k][j];
        }
      }
    }
    res = newRes;
  }
  return res;
}

/**
 * @param {Mat3} m 
 * @param {Vec2} v 
 * @returns {Vec2}
 */
export function poop(m, v) {
  return unembed(apply(m, embed(v)));
}

/**
 * @param {Mat3} m 
 * @param {Vec3} v 
 * @returns {Vec3}
 */
export function apply(m, v) {
  return [
    m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
    m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
    m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2],
  ];
}

/**
 * @param {Mat3} m 
 * @returns {Mat3}
 */
export function invert(m) {
  const [
    [a, b, c],
    [d, e, f],
    [g, h, i],
  ] = m;
  const det = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g);
  const invDet = 1/det;
  return compose([
    [e*i - f*h, c*h - b*i, b*f - c*e],
    [f*g - d*i, a*i - c*g, c*d - a*f],
    [d*h - e*g, b*g - a*h, a*e - b*d],
  ], [
    [invDet,      0,      0],
    [     0, invDet,      0],
    [     0,      0, invDet],
  ]);
}

/**
 * @param {Vec2} v 
 * @returns {Mat3}
 */
export function translate(v) {
  return [
    [1, 0, v[0]],
    [0, 1, v[1]],
    [0, 0,    1],
  ];
}

/**
 * @param {Vec2} v 
 * @returns {Mat3}
 */
export function scaleXY(v) {
  return [
    [v[0],    0, 0],
    [   0, v[1], 0],
    [   0,    0, 1],
  ];
}

/**
 * @param {number} coeff
 * @returns {Mat3}
 */
export function scaleX(coeff) {
  return scaleXY([coeff, 1]);
}

/**
 * @param {number} coeff
 * @returns {Mat3}
 */
export function scaleY(coeff) {
  return scaleXY([1, coeff]);
}

/**
 * @param {number} coeff
 * @returns {Mat3}
 */
export function scale(coeff) {
  return scaleXY([coeff, coeff]);
}

const EPS = 1e-6;
/**
 * intersect two line segments v1-v2 and w1-w2
 * @param {[Vec2, Vec2]} param0 
 * @param {[Vec2, Vec2]} param1 
 * @returns {Vec2 | null}
 */
export function intersect([v1, v2], [w1, w2]) {
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

/**
 * @param {number} x 
 * @param {number} y 
 * @returns {[number, number]}
 */
export function minmax(x, y) {
  return x < y ? [x, y] : [y, x];
}

/**
 * @param {Vec2} v 
 * @param {Vec2} w 
 * @returns {Vec2}
 */
export function minus(v, w) {
  return [v[0] - w[0], v[1] - w[1]];
}

/**
 * @param {Vec2} v 
 * @param {Vec2} w 
 * @returns {Vec2}
 */
function plus(v, w) {
  return [v[0] + w[0], v[1] + w[1]];
}

/**
 * @param {Vec2} v 
 * @param {number} coeff 
 * @returns {Vec2}
 */
function multiply(v, coeff) {
  return [v[0] * coeff, v[1] * coeff];
}

/**
 * @param {Vec2} v 
 * @param {Vec2} w 
 * @returns {number}
 */
function cross(v, w) {
  return v[0] * w[1] - v[1] * w[0];
}

/**
 * @param {Vec2} v 
 * @param {Vec2} w 
 * @returns {number}
 */
function dot(v, w) {
  return v[0] * w[0] + v[1] * w[1];
}

/**
 * @param {Vec2} v 
 * @returns {number}
 */
export function distSq(v) {
  return dot(v, v);
}
