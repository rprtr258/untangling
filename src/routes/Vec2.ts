type Vec2 = {
  x: number,
  y: number,
};

function minus(v: Vec2, w: Vec2): Vec2 {
  return {x: v.x - w.x, y: v.y - w.y}
}

function plus(v: Vec2, w: Vec2): Vec2 {
  return {x: v.x + w.x, y: v.y + w.y}
}

function multiply(v: Vec2, c: number): Vec2 {
  return {x: v.x * c, y: v.y * c};
}

function cross(v: Vec2, w: Vec2): number {
  return v.x * w.y - v.y * w.x;
}

function dot(v: Vec2, w: Vec2): number {
  return v.x * w.x + v.y * w.y;
}

export {Vec2, minus, plus, multiply, cross, dot};

