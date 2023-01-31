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

export function combine(first: Mat3, second: Mat3): Mat3 {
  let res = [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      for (let k = 0; k < 3; k++) {
        res[i][j] += second[i][k] * first[k][j];
      }
    }
  }
  return res as Mat3;
}

export function apply(m: Mat3, v: Vec3): Vec3 {
  return [
    m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
    m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
    m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2],
  ];
}

export function translate(v: Vec2): Mat3 {
  return [
    [1, 0, v[0]],
    [0, 1, v[1]],
    [0, 0, 1],
  ];
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

export function multiply(v: Vec2, c: number): Vec2 {
  return [v[0] * c, v[1] * c];
}

export function cross(v: Vec2, w: Vec2): number {
  return v[0] * w[1] - v[1] * w[0];
}

export function dot(v: Vec2, w: Vec2): number {
  return v[0] * w[0] + v[1] * w[1];
}

export function distSq(v: Vec2): number {
  return dot(v, v);
}
