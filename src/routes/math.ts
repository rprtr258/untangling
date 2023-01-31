export type Mat3 = [
  [number, number, number],
  [number, number, number],
  [number, number, number]
];

export type Vec2 = [number, number];

export type Vec3 = [number, number, number];

export function embed(v: Vec2): Vec3 {
  return [v[0], v[1], 1];
}

export function unembed(v: Vec3): Vec2 {
  return [x(v), y(v)];
}

export function x(v: Vec3): number {
  return v[0] / v[2];
}

export function y(v: Vec3): number {
  return v[1] / v[2];
}

export function compose(...ms: Mat3[]): Mat3 {
  let res = [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1],
  ];
  for (let m of ms) {
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
  return res as Mat3;
}

export function poop(m: Mat3, v: Vec2): Vec2 {
  return unembed(apply(m, embed(v)));
}

export function apply(m: Mat3, v: Vec3): Vec3 {
  return [
    m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
    m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
    m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2],
  ];
}

export function invert(m: Mat3): Mat3 {
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

export function translate(v: Vec2): Mat3 {
  return [
    [1, 0, v[0]],
    [0, 1, v[1]],
    [0, 0,    1],
  ];
}

export function scaleXY(v: Vec2): Mat3 {
  return [
    [v[0],    0, 0],
    [   0, v[1], 0],
    [   0,    0, 1],
  ];
}

export function scaleX(coeff: number): Mat3 {
  return scaleXY([coeff, 1]);
}

export function scaleY(coeff: number): Mat3 {
  return scaleXY([1, coeff]);
}

export function scale(coeff: number): Mat3 {
  return scaleXY([coeff, coeff]);
}

const EPS = 1e-6;
// intersect two line segments v1-v2 and w1-w2
export function intersect([v1, v2]: [Vec2, Vec2], [w1, w2]: [Vec2, Vec2]): Vec2 | null {
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

// =============== old ==============

export function minus(v: Vec2, w: Vec2): Vec2 {
  return unembed(apply(
    translate([-w[0], -w[1]]),
    embed(v),
  ));
}

export function plus(v: Vec2, w: Vec2): Vec2 {
  return unembed(apply(
    translate(w),
    embed(v),
  ));
}

function multiply(v: Vec2, coeff: number): Vec2 {
  return unembed(apply(
    scale(coeff),
    embed(v),
  ));
}

function cross(v: Vec2, w: Vec2): number {
  return v[0] * w[1] - v[1] * w[0];
}

export function dot(v: Vec2, w: Vec2): number {
  return v[0] * w[0] + v[1] * w[1];
}

export function distSq(v: Vec2): number {
  return dot(v, v);
}
