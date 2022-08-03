module Vec2 exposing (Vec2, distSquared, dist, plus, minus, multiply)

type alias Vec2 = (Float, Float)

distSquared : Vec2 -> Vec2 -> Float
distSquared (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

dist : Vec2 -> Vec2 -> Float
dist v w = sqrt (distSquared v w)

plus : Vec2 -> Vec2 -> Vec2
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minus : Vec2 -> Vec2 -> Vec2
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

multiply : Float -> Vec2 -> Vec2
multiply k (x, y) = (x * k, y * k)
