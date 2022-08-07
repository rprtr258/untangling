module Main exposing (main)

import Set
import Array
import Random

import Vec2
import Engine
import Time


-- TODO: move to engine
type MouseState = Up | Down
type alias VertexIdx = Int
type alias Vertex = Vec2.Vec2 -- TODO: coords in [-1, -1] x [1, 1]
type alias Model = {
  mouse: MouseState,
  heldVertexIdx: Maybe VertexIdx,
  vertices: Array.Array (Vertex, List VertexIdx),
  intersections: Set.Set Vec2.Vec2
  }

vertexRadius : Float
vertexRadius = 20

attractionG : Float
attractionG = 10

initModel : Model
initModel = generateModel (Random.initialSeed 12345) 5

generateModel : Random.Seed -> Int -> Model
generateModel r n =
  let
    floatGenerator = Random.float -400 400
    (x0, r1) = (Random.step floatGenerator r)
    (y0, r2) = (Random.step floatGenerator r1)
    vertices = n - 1
      |> List.range 1
      |> List.foldl (\i (xs, rr) ->
        let
          (x, rr1) = (Random.step floatGenerator rr)
          (y, rr2) = (Random.step floatGenerator rr1)
        in
          (xs ++ [((x, y), List.range 0 (i - 1))], rr2)
        ) ([((x0, y0), [])], r2)
      |> Tuple.first
    -- vertices = [
    --   ((0, 300), [1, 4]),
    --   ((400, -150), [2, 0]),
    --   ((-400, 150), [3, 1]),
    --   ((450, 150), [4, 2]),
    --   ((-400, -150), [0, 3])
    --   ]
  in
    {
      mouse = Up,
      vertices = vertices |> Array.fromList,
      heldVertexIdx = Nothing,
      intersections = Set.empty
      }

-- main : Program () (Engine.Game Model) Msg
main = Engine.game myRender myUpdate initModel

applyTransforms : List (Engine.Transform -> Engine.Transform) -> Engine.Shape -> Engine.Shape
applyTransforms fs shape = {shape | transform = (List.foldl (\f g -> \x -> x |> g |> f) identity fs) shape.transform}

myRender : Engine.Computer -> Model -> List Engine.Shape
myRender computer model =
  let
    background = Engine.rectangle Engine.palette.darkCharcoal computer.screen.width computer.screen.height
    intersectionsText = model.intersections
      |> Set.size
      |> String.fromInt
      |> Engine.words Engine.palette.white
      |> applyTransforms [Engine.move 0 (computer.screen.top - 20)]
    edges = (
      let
        (heldEdges, notHeldEdges) = iterEdgesEnds model.vertices
          |> List.concatMap (\(i, vi, tos) -> tos
            |> List.map (\(j, to) -> ((i, j), (vi, to))))
          |> List.partition (\((i, j), _) -> model.heldVertexIdx
            |> Maybe.map (\k -> i == k || j == k)
            |> Maybe.withDefault False)
        colorEdges c ls = ls
          |> List.map Tuple.second
          |> List.map (\(x, y) -> Engine.path c [x, y])
      in
        (colorEdges Engine.palette.black notHeldEdges) ++
        (colorEdges (Engine.Hex "#505060") heldEdges)
      )
    vertices = model.vertices
      |> Array.map Tuple.first
      |> Array.map (\(x, y) -> Engine.circle Engine.palette.darkGrey vertexRadius |> applyTransforms [Engine.move x y])
      |> Array.toList
    -- TODO: fix lag on moving
    intersections = model.intersections
      |> Set.toList
      |> List.map (\(x, y) -> Engine.circle Engine.palette.red 5 |> applyTransforms [Engine.move x y])
  in
    background :: intersectionsText :: edges ++ vertices ++ intersections

myUpdate : Engine.Computer -> Model -> Model
myUpdate computer model =
  let
    newMouseState = updateMouseState computer.mouse model.mouse
    -- TODO: 2 modes: down=take, up=release or click=take, click again=release
    newIdx = case (model.mouse, newMouseState) of
      (Up, Down) -> chooseVertex (model.vertices |> Array.map Tuple.first |> Array.toList |> List.indexedMap Tuple.pair) (computer.mouse.x, computer.mouse.y)
      (Down, Up) -> Nothing
      _ -> model.heldVertexIdx
    totalVertices = Array.length model.vertices
    forces = if
        computer.keyboard.space
      then
        iterEdgesEnds model.vertices
          |> List.map (\(_, iv, tos) -> tos
            |> List.map Tuple.second
            |> List.map (\jv ->
              let
                dv = Vec2.minus jv iv
                coeff = attractionG / (Vec2.dist jv iv) / 2.8
              in
                Vec2.multiply coeff dv))
          -- |> List.map (\(fs, v, tos) -> fs ++ (tos
          --   |> List.filterMap (\j -> Array.get j model.vertices)
          --   |> List.map Tuple.first
          --   |> List.map (\to ->
          --     let
          --       dv = minus to v
          --       coeff = attractionG / (dist to v)
          --     in
          --       multiply coeff dv))
          --   )
          |> List.map (\fs -> List.foldl Vec2.plus (0, 0) fs)
      else
        List.map (\_ -> (0, 0)) (List.range 0 totalVertices)
    movedVertices = model.vertices
      |> Array.toList
      |> List.map2 (\f (v, tos) -> (Vec2.plus f v, tos)) forces
      |> Array.fromList
    updateHeldVertex vertices = case newIdx of
      Just i -> case Array.get i vertices of
        Just (_, tos) -> Array.set i ((computer.mouse.x, computer.mouse.y), tos) vertices
        _ -> vertices
      Nothing -> vertices
    -- TODO: OPTIMIZE: recalculate only moved edges
    edges = iterEdgesEnds movedVertices
    newIntersections = edges
      |> List.concatMap (\ei -> edges |> List.map (\ej -> (ei, ej)))
      |> List.filter (\((i, _, _), (j, _, _)) -> i < j)
      |> List.concatMap (\((i, iv, itos), (j, jv, jtos)) -> itos
        |> List.concatMap (\(ii, iiv) -> jtos
          |> List.map (\(jj, jjv) -> (((i, ii), (j, jj)), ((iv, iiv), (jv, jjv))))))
      |> List.filter (\(((i, ii), (j, jj)), _) -> i < ii && j < jj)
      |> List.filter (\(((i, ii), (j, jj)), _) -> i /= j && ii /= j && i /= jj && ii /= jj)
      |> List.map Tuple.second
      |> List.filterMap intersectEdges
      |> Set.fromList
  in {
    mouse = newMouseState,
    vertices = updateHeldVertex movedVertices,
    heldVertexIdx = newIdx,
    intersections = newIntersections
    }




intersectEdges : ((Vertex, Vertex), (Vertex, Vertex)) -> Maybe Vec2.Vec2
intersectEdges ((v1, v2), (w1, w2)) =
  let
    dw = Vec2.minus w2 w1
    dv = Vec2.minus v2 v1
    dvw1 = Vec2.minus v1 w1
    denom = Vec2.cross dv dw
  in
    if denom == 0 then -- collinear
      Nothing
    else
      let
        ua = (Vec2.cross dw dvw1) / denom
        ub = (Vec2.cross dv dvw1) / denom
      in
        if ua < 0 || ua > 1 || ub < 0 || ub > 1 then -- out of range
          Nothing
        else
          Just (Vec2.plus v1 (Vec2.multiply ua dv))







indexedVertices : Array.Array (Vertex, a) -> List (VertexIdx, Vertex)
indexedVertices vertices = vertices
  |> Array.toList
  |> List.indexedMap (\i (v, _) -> (i, v))

iterVerticesPairs : Array.Array (Vertex, a) -> List ((VertexIdx, Vertex), (VertexIdx, Vertex))
iterVerticesPairs vertices = vertices
  |> indexedVertices
  |> List.concatMap (\iv -> vertices
    |> indexedVertices
    |> List.map (\jw -> (iv, jw)))
  |> List.filter (\((i, _), (j, _)) -> (i /= j))

iterEdgesEnds : Array.Array (Vertex, List VertexIdx) -> List (VertexIdx, Vertex, List (VertexIdx, Vertex))
iterEdgesEnds vertices = vertices
  |> Array.toList
  |> List.indexedMap (\i (v, tos) -> (i, v, tos))
  |> List.map (\(i, iv, itos) -> (i, iv, itos
    |> List.filterMap (\j -> Maybe.map (\(jv, _) -> (j, jv)) (Array.get j vertices))))

updateMouseState : Engine.Mouse -> MouseState -> MouseState
updateMouseState mouse model = case (mouse.down, mouse.click) of
  (True, _) -> Down
  (_, True) -> Up
  (_, False) -> model

-- TODO: take closest
chooseVertex : List (VertexIdx, Vertex) -> (Float, Float) -> Maybe VertexIdx
chooseVertex vertices pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
















