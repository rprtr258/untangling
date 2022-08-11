module Main exposing (main)

import Set

import Random
import Graph

import Vec2
import Engine
import IntDict
import Random.List


-- TODO: move to engine
type MouseState = Up | Down
type alias Vertex = Vec2.Vec2 -- TODO: coords in [-1, -1] x [1, 1]
type alias IntersectionTuple = (
  (Graph.NodeId, Graph.NodeId),
  (Graph.NodeId, Graph.NodeId),
  Vec2.Vec2
  )
type alias Intersection = {
  from1: Graph.NodeId,
  to1: Graph.NodeId,
  from2: Graph.NodeId,
  to2: Graph.NodeId,
  pt: Vec2.Vec2
  }

intersectionFromTuple : IntersectionTuple -> Intersection
intersectionFromTuple ((from1, to1), (from2, to2), pt) = {
  from1 = from1,
  to1 = to1,
  from2 = from2,
  to2 = to2,
  pt = pt
  }

intersectionToTuple : Intersection -> IntersectionTuple
intersectionToTuple {from1, to1, from2, to2, pt} = ((from1, to1), (from2, to2), pt)

type alias Model = {
  mouse: MouseState,
  heldVertexIdx: Maybe Graph.NodeId,
  graph: Graph.Graph Vertex (Vertex, Vertex),
  intersections: Set.Set IntersectionTuple
  }

vertexRadius : Float
vertexRadius = 10

attractionG : Float
attractionG = 4

initModel : Model
initModel =
  let
    graph = generateGraph (Random.initialSeed 12345) 20
    intersections = graph
      |> Graph.edges
      |> squareList
      |> List.filter (\(e1, e2) -> (e1.from /= e2.from && e1.from /= e2.to && e1.to /= e2.from && e1.to /= e2.to))
      |> List.filter (\(e1, e2) -> (e1.from < e2.from || e1.from == e2.from && e1.to < e2.to))
      |> List.filterMap (\(e1, e2) -> (intersectEdges e1.label e2.label) |> Maybe.map (\pt -> intersectionToTuple {
        from1 = e1.from,
        to1 = e1.to,
        from2 = e2.from,
        to2 = e2.to,
        pt = pt
        }))
      |> Set.fromList
  in {
    mouse = Up,
    graph = graph,
    heldVertexIdx = Nothing,
    intersections = intersections
    }

generateGraph : Random.Seed -> Int -> Graph.Graph Vertex (Vertex, Vertex)
generateGraph r n =
  let
    floatGenerator = Random.float -400 400
    (vertices, rrrrr) = n
      |> List.range 1
      |> List.foldl (\_ (xs, rrr) ->
        let
          (x, rr1) = (Random.step floatGenerator rrr)
          (y, rr2) = (Random.step floatGenerator rr1)
        in
          ((x, y) :: xs, rr2)) ([], r)
      |> (\(xys, rt) ->
        let
          ixys = xys
            |> List.indexedMap Tuple.pair
            |> IntDict.fromList
        in
          (ixys, rt))
    (vertices2, rrrrrr) = n
      |> List.range 1
      |> List.foldl (\_ (xs, rrr) ->
        let
          (x, rr1) = (Random.step floatGenerator rrr)
          (y, rr2) = (Random.step floatGenerator rr1)
        in
          ((x, y) :: xs, rr2)) ([], rrrrr)
      |> (\(xys, rt) ->
        let
          ixys = xys
            |> List.indexedMap Tuple.pair
            |> IntDict.fromList
        in
          (ixys, rt))
    -- probGenerator = Random.float 0 1
  in
    n - 1
      |> List.range 0
      |> squareList
      |> List.filter (\(i, j) -> i < j)
      |> (\edges -> (Random.step (Random.List.shuffle edges) rrrrrr))
      |> Tuple.first
      |> List.map (\(i, j) ->
        let
          -- TODO: no default
          vi = vertices |> IntDict.get i |> Maybe.withDefault (0, 0)
          vj = vertices |> IntDict.get j |> Maybe.withDefault (0, 0)
        in
          ((i, j), (vi, vj)))
      |> List.foldl (\((i, j), vij) edges ->
        if
          edges
            |> List.filter (\((k, l), _) -> i /= k && i /= l && j /= k && j /= l)
            |> List.map Tuple.second
            |> List.map (intersectEdges vij)
            |> List.any isJust
        then
          edges
        else
          ((i, j), vij) :: edges)
        []
      |> List.map (\((i, j), _) ->
        let
          -- TODO: no default
          vi = vertices2 |> IntDict.get i |> Maybe.withDefault (0, 0)
          vj = vertices2 |> IntDict.get j |> Maybe.withDefault (0, 0)
        in
          ((i, j), (vi, vj)))
      |> List.map (\((from, to), vij) -> Graph.Edge from to vij)
      |> Graph.fromNodesAndEdges (vertices2 |> IntDict.values |> List.indexedMap Graph.Node)

isJust : Maybe a -> Bool
isJust x = case x of
  Just _ -> True
  Nothing -> False

squareList : List a -> List (a, a)
squareList xs = xs
  |> List.concatMap (\x -> xs |> List.map (Tuple.pair x))

main : Program () (Engine.Game Model) Engine.Msg
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
          |> List.map .label
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
      |> List.map intersectionFromTuple
      |> List.map .pt
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
    -- totalVertices = Graph.size model.graph
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
          newPos = (computer.mouse.x, computer.mouse.y)
          vs = model.graph
            |> Graph.nodes
            |> List.map (\n -> {n | label = if n.id == i then newPos else n.label})
          es = model.graph
            |> Graph.edges
            |> List.map (\e -> {e | label =
              if e.from == i
                then (newPos, Tuple.second e.label)
              else if e.to == i
                then (Tuple.first e.label, newPos)
              else e.label})
        in
          Graph.fromNodesAndEdges vs es
      Nothing -> model.graph
    edges2 = movedVertices
      |> Graph.edges
      |> List.map (\{from, to, label} -> ((from, to), label))
    updatedIntersections = newIdx
      |> Maybe.andThen (\idx -> Graph.get idx movedVertices)
      |> Maybe.map (\ctx ->
        let
          tos = (Graph.alongOutgoingEdges ctx) ++ (Graph.alongIncomingEdges ctx)
          idx = ctx.node.id
        in
          tos |> List.map (Tuple.pair idx)
      )
      |> Maybe.withDefault []
      |> List.filterMap (\(from, to) ->
        let
          fromV = movedVertices |> Graph.get from |> Maybe.map (\{node} -> node.label) |> Maybe.withDefault (0, 0)
          toV = movedVertices |> Graph.get to |> Maybe.map (\{node} -> node.label) |> Maybe.withDefault (0, 0)
        in
          Just ((from, to), (fromV, toV))
      )
      |> List.concatMap (\e -> List.map (Tuple.pair e) edges2)
      |> List.filter (\(((from1, to1), _), ((from2, to2), _)) -> (from1 /= from2 && from1 /= to2 && to1 /= from2 && to1 /= to2))
      |> List.filterMap (\(((from1, to1), e1), ((from2, to2), e2)) -> Maybe.map (\i -> ((from1, to1), (from2, to2), i)) (intersectEdges e1 e2))
      |> Set.fromList
    newIntersections = case newIdx of
      Just i -> model.intersections
        |> Set.filter (\((a, b), (c, d), _) -> a /= i && b /= i && c /= i && d /= i)
        |> Set.union updatedIntersections
      Nothing -> model.intersections
  in {
    mouse = newMouseState,
    graph = movedVertices,
    heldVertexIdx = newIdx,
    intersections = newIntersections
    }

intersectEdges : (Vertex, Vertex) -> (Vertex, Vertex) -> Maybe Vec2.Vec2
intersectEdges (v1, v2) (w1, w2) =
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

-- TODO: fix mouse sticking
updateMouseState : Engine.Mouse -> MouseState -> MouseState
updateMouseState mouse model = case (mouse.down, mouse.click) of
  (True, _) -> Down
  (_, True) -> Up
  (_, False) -> model

-- TODO: take closest
chooseVertex : List (Graph.NodeId, Vertex) -> Vec2.Vec2 -> Maybe Graph.NodeId
chooseVertex vertices pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
