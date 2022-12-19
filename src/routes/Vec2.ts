export type Vec2 = {
  x: number,
  y: number,
};

export function minus(v: Vec2, w: Vec2): Vec2 {
  return {x: v.x - w.x, y: v.y - w.y}
}

export function plus(v: Vec2, w: Vec2): Vec2 {
  return {x: v.x + w.x, y: v.y + w.y}
}

export function multiply(v: Vec2, c: number): Vec2 {
  return {x: v.x * c, y: v.y * c};
}

export function cross(v: Vec2, w: Vec2): number {
  return v.x * w.y - v.y * w.x;
}

export function dot(v: Vec2, w: Vec2): number {
  return v.x * w.x + v.y * w.y;
}

export function distSquared(v: Vec2): number {
  return dot(v, v);
}

