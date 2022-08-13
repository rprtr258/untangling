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
  graphicsConfig: GraphicsConfig,
  mouse: MouseState,
  heldVertexIdx: Maybe Graph.NodeId,
  graph: Graph.Graph Vertex (Vertex, Vertex),
  intersections: Set.Set IntersectionTuple
  }

type alias GraphicsConfig = {
  vertexRadius: Engine.Number,
  vertexColor: Engine.Color,
  edgeWidth: Engine.Number,
  intersectionRadius: Engine.Number,
  intersectionColor: Engine.Color,
  heldEdgeColor: Engine.Color,
  notHeldEdgeColor: Engine.Color,
  textColor: Engine.Color,
  backgroundColor: Engine.Color
  }

initGraphicsConfig : GraphicsConfig
initGraphicsConfig = {
  vertexRadius = 10,
  vertexColor = Engine.palette.darkGrey,
  edgeWidth = 3,
  heldEdgeColor = Engine.Hex "#505060",
  notHeldEdgeColor = Engine.palette.black,
  intersectionRadius = 2,
  intersectionColor = Engine.palette.red,
  textColor = Engine.palette.white,
  backgroundColor = Engine.palette.darkCharcoal
  }

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
    intersections = intersections,
    graphicsConfig = initGraphicsConfig
    }

generateGraph : Random.Seed -> Int -> Graph.Graph Vertex (Vertex, Vertex)
generateGraph r0 n =
  let
    floatGenerator = Random.float -400 400
    (vertices, r2) = n
      |> List.range 1
      |> List.foldl (\_ (xs, r1) ->
        let
          (x, r2_1) = (Random.step floatGenerator r1)
          (y, r2_2) = (Random.step floatGenerator r2_1)
        in
          ((x, y) :: xs, r2_2)) ([], r0)
      |> (\(xys, r3) ->
        let
          ixys = xys
            |> List.indexedMap Tuple.pair
            |> IntDict.fromList
        in
          (ixys, r3))
    (vertices2, r5) = n
      |> List.range 1
      |> List.foldl (\_ (xs, rrr) ->
        let
          (x, rr1) = (Random.step floatGenerator rrr)
          (y, rr2) = (Random.step floatGenerator rr1)
        in
          ((x, y) :: xs, rr2)) ([], r2)
      |> (\(xys, r4) ->
        let
          ixys = xys
            |> List.indexedMap Tuple.pair
            |> IntDict.fromList
        in
          (ixys, r4))
  in
    n - 1
      |> List.range 0
      |> squareList
      |> List.filter (\(i, j) -> i < j)
      |> (\edges -> (Random.step (Random.List.shuffle edges) r5))
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
    background = Engine.rectangle model.graphicsConfig.backgroundColor screen.width screen.height
    intersectionsText = model.intersections
      |> Set.size
      |> String.fromInt
      |> Engine.words model.graphicsConfig.textColor
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
          |> List.map (\(x, y) -> Engine.path c model.graphicsConfig.edgeWidth [x, y])
        in
          (colorEdges model.graphicsConfig.notHeldEdgeColor notHeldEdges) ++
            (colorEdges model.graphicsConfig.heldEdgeColor heldEdges)
      )
    vertices = model.graph
      |> Graph.nodes
      |> List.map .label
      |> List.map (\(x, y) -> Engine.circle model.graphicsConfig.vertexColor model.graphicsConfig.vertexRadius |> applyTransforms [Engine.move x y])

    intersectionCircle = Engine.circle model.graphicsConfig.intersectionColor model.graphicsConfig.intersectionRadius
    intersections = model.intersections
      |> Set.toList
      |> List.map intersectionFromTuple
      |> List.map .pt
      |> List.map (\(x, y) -> intersectionCircle |> applyTransforms [Engine.move x y])
  in
    background :: edges ++ vertices ++ intersections ++ [intersectionsText]

myUpdate : Engine.Computer -> Model -> Model
myUpdate computer model =
  let
    newMouseState = updateMouseState computer.mouse
    -- TODO: 2 modes: down=take, up=release or click=take, click again=release
    newIdx = case (model.mouse, newMouseState) of
      (Up, Down) -> chooseVertex
        (model.graph
          |> Graph.nodes
          |> List.map (\{id, label} -> (id, label)))
        model.graphicsConfig.vertexRadius
        computer.mouse.pos
      (Down, Up) -> Nothing
      _ -> model.heldVertexIdx
    movedVertices = case newIdx of
      Just i ->
        let
          newPos = computer.mouse.pos
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
  in {model |
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

updateMouseState : Engine.Mouse -> MouseState
updateMouseState mouse = case (mouse.down, mouse.click) of
  (True, _) -> Down
  _ -> Up

-- TODO: take closest
chooseVertex : List (Graph.NodeId, Vertex) -> Engine.Number -> Vec2.Vec2 -> Maybe Graph.NodeId
chooseVertex vertices vertexRadius pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
