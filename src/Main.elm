module Main exposing (main)

import Set
import Array

import Random
import Graph

import Vec2
import Engine


-- TODO: move to engine
type MouseState = Up | Down
type alias Vertex = Vec2.Vec2 -- TODO: coords in [-1, -1] x [1, 1]
type alias Model = {
  mouse: MouseState,
  heldVertexIdx: Maybe Graph.NodeId,
  graph: Graph.Graph Vertex (),
  intersections: Set.Set Vec2.Vec2
  }

vertexRadius : Float
vertexRadius = 10

attractionG : Float
attractionG = 4

initModel : Model
initModel = generateModel (Random.initialSeed 12345) 20

generateModel : Random.Seed -> Int -> Model
generateModel r n =
  let
    floatGenerator = Random.float -400 400
    probGenerator = Random.float 0 1
    (x0, r1) = (Random.step floatGenerator r)
    (y0, r2) = (Random.step floatGenerator r1)
    graph = n - 1
      |> List.range 1
      |> List.foldl (\i (xs, edges, rr) ->
        let
          (x, rr1) = (Random.step floatGenerator rr)
          (y, rr2) = (Random.step floatGenerator rr1)
          (newEdges, rr3) = i - 1
            |> List.range 1
            |> List.foldl (\j (ys, rrr) ->
              let
                (p, rrr1) = Random.step probGenerator rrr
                dys = if p < 0.10 then [j] else []
              in
                (ys ++ dys, rrr1)) ([], rr2)
        in
          (xs ++ [(x, y)], Set.union edges (newEdges
            |> List.map (\j -> (i, j))
            |> Set.fromList
            ) , rr3))
        ([(x0, y0)], Set.empty, r2)
      |> (\(positions, edges, _) -> (positions, edges))
      |> (\(positions, edges) -> positions
        |> List.indexedMap Tuple.pair
        |> List.foldl (\(i, v) g -> g ++ [(v, edges
          |> Set.filter (\(j, k) -> j == i || k == i)
          |> Set.map (\(j, k) -> if j == i then k else j)
          |> Set.toList)]) [])
      |> (\xs ->
        let
          vs = xs
            |> List.map Tuple.first
            |> List.indexedMap Graph.Node
          es = xs
            |> List.indexedMap (\i (_, tos) -> (i, tos))
            |> List.concatMap (\(from, tos) -> tos |> List.map (\to -> Graph.Edge from to ()))
        in
          Graph.fromNodesAndEdges vs es)
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
      graph = graph,
      heldVertexIdx = Nothing,
      intersections = Set.empty
      }

-- main : Program () (Engine.Game Model) Msg
main = Engine.game myRender myUpdate initModel

applyTransforms : List (Engine.Transform -> Engine.Transform) -> Engine.Shape -> Engine.Shape
applyTransforms fs shape = {shape | transform = (List.foldl (\f g -> \x -> x |> g |> f) identity fs) shape.transform}

myRender : Engine.Screen -> Model -> List Engine.Shape
myRender screen model =
  let
    background = Engine.rectangle Engine.palette.darkCharcoal screen.width screen.height
    intersectionsText = model.intersections
      |> Set.size
      |> String.fromInt
      |> Engine.words Engine.palette.white
      |> applyTransforms [Engine.move 0 (screen.top - 20)]
    edges = (
      let
        (heldEdges, notHeldEdges) = case model.heldVertexIdx of
          Maybe.Just idx -> model.graph
            |> Graph.edges
            |> List.partition (\{from, to} -> from == idx || to == idx)
          Nothing -> ([], Graph.edges model.graph)
        colorEdges c ls = ls
          |> List.map (\{from, to} ->
            let
              fromV = model.graph |> Graph.get from |> Maybe.map (\{node} -> node.label) |> Maybe.withDefault (0, 0)
              toV = model.graph |> Graph.get to |> Maybe.map (\{node} -> node.label) |> Maybe.withDefault (0, 0)
              -- Maybe.andThen (Maybe.map) fromV
            in
              (fromV, toV))
          |> List.map (\(x, y) -> Engine.path c [x, y])
        in
          (colorEdges Engine.palette.black notHeldEdges) ++
            (colorEdges (Engine.Hex "#505060") heldEdges)
      )
    vertices = model.graph
      |> Graph.nodes
      |> List.map .label
      |> List.map (\(x, y) -> Engine.circle Engine.palette.darkGrey vertexRadius |> applyTransforms [Engine.move x y])
    -- TODO: fix lag on moving
    intersections = model.intersections
      |> Set.toList
      |> List.map (\(x, y) -> Engine.circle Engine.palette.red 3 |> applyTransforms [Engine.move x y])
  in
    background :: edges ++ vertices ++ intersections ++ [intersectionsText]

myUpdate : Engine.Computer -> Model -> Model
myUpdate computer model =
  let
    newMouseState = updateMouseState computer.mouse model.mouse
    -- TODO: 2 modes: down=take, up=release or click=take, click again=release
    newIdx = case (model.mouse, newMouseState) of
      (Up, Down) -> chooseVertex (model.graph |> Graph.nodes |> List.map (\{id, label} -> (id, label))) (computer.mouse.x, computer.mouse.y)
      (Down, Up) -> Nothing
      _ -> model.heldVertexIdx
    totalVertices = Graph.size model.graph
    -- forces = if
    --     computer.keyboard.space || computer.keyboard.enter
    --   then
    --     let
    --       g = if computer.keyboard.space then attractionG else -attractionG
    --     in
    --       []
    --       -- model.graph
    --       --   |> Graph.edges
    --       --   |> List.map (\(_, iv, tos) -> tos
    --       --     |> List.map Tuple.second
    --       --     |> List.map (\jv ->
    --       --       let
    --       --         dv = Vec2.minus jv iv
    --       --         coeff = g / (Vec2.dist jv iv) / 2.8
    --       --       in
    --       --         Vec2.multiply coeff dv))
    --         -- |> List.map (\(fs, v, tos) -> fs ++ (tos
    --         --   |> List.filterMap (\j -> Array.get j model.vertices)
    --         --   |> List.map Tuple.first
    --         --   |> List.map (\to ->
    --         --     let
    --         --       dv = minus to v
    --         --       coeff = attractionG / (dist to v)
    --         --     in
    --         --       multiply coeff dv))
    --         --   )
    --         |> List.map (\fs -> List.foldl Vec2.plus (0, 0) fs)
    --   else
    --     List.map (\_ -> (0, 0)) (List.range 0 totalVertices)
    -- movedVertices = model.graph
      -- |> Graph.mapNodes (\
      -- |> List.map2 (\f (v, tos) -> (Vec2.plus f v, tos)) forces
      -- |> Array.fromList
    movedVertices = case newIdx of
      Just i ->
        let
          g ctx = Graph.NodeContext (Graph.Node i (computer.mouse.x, computer.mouse.y)) ctx.incoming ctx.outgoing
          f = Maybe.map g
        in
          Graph.update i f model.graph
      Nothing -> model.graph
    -- -- TODO: OPTIMIZE: recalculate only moved edges
    -- edges = iterEdgesEnds movedVertices
    -- newIntersections = edges
    --   |> List.concatMap (\ei -> edges |> List.map (\ej -> (ei, ej)))
    --   |> List.filter (\((i, _, _), (j, _, _)) -> i < j)
    --   |> List.concatMap (\((i, iv, itos), (j, jv, jtos)) -> itos
    --     |> List.concatMap (\(ii, iiv) -> jtos
    --       |> List.map (\(jj, jjv) -> (((i, ii), (j, jj)), ((iv, iiv), (jv, jjv))))))
    --   |> List.filter (\(((i, ii), (j, jj)), _) -> i < ii && j < jj)
    --   |> List.filter (\(((i, ii), (j, jj)), _) -> i /= j && ii /= j && i /= jj && ii /= jj)
    --   |> List.map Tuple.second
    --   |> List.filterMap intersectEdges
    --   |> Set.fromList
  in {
    mouse = newMouseState,
    graph = movedVertices,
    heldVertexIdx = newIdx,
    intersections = model.intersections --newIntersections
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







indexedVertices : Array.Array (Vertex, a) -> List (Graph.NodeId, Vertex)
indexedVertices vertices = vertices
  |> Array.toList
  |> List.indexedMap (\i (v, _) -> (i, v))

iterVerticesPairs : Array.Array (Vertex, a) -> List ((Graph.NodeId, Vertex), (Graph.NodeId, Vertex))
iterVerticesPairs vertices = vertices
  |> indexedVertices
  |> List.concatMap (\iv -> vertices
    |> indexedVertices
    |> List.map (\jw -> (iv, jw)))
  |> List.filter (\((i, _), (j, _)) -> (i /= j))

iterEdgesEnds : Array.Array (Vertex, List Graph.NodeId) -> List (Graph.NodeId, Vertex, List (Graph.NodeId, Vertex))
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
chooseVertex : List (Graph.NodeId, Vertex) -> (Float, Float) -> Maybe Graph.NodeId
chooseVertex vertices pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
















