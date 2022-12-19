type Vec2 = {
  x: number,
  y: number,
};

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

export {Vec2, minus, plus, multiply, cross};

