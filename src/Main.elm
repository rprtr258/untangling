module Main exposing (main)

import Random
import Graph

import Vec2
import Engine
import IntDict
import Random.List


-- TODO: move to engine
type MouseState = Up | Vertex Graph.NodeId | CameraMove Vec2.Vec2
type alias Vertex = Vec2.Vec2 -- TODO: coords in [-1, -1] x [1, 1]

type alias Edge = (Graph.NodeId, Graph.NodeId)

toEdge : Graph.Edge a -> Edge
toEdge {from, to} = (from, to)

type alias Intersection = {
  first: Edge,
  second: Edge,
  pt: Vec2.Vec2
  }

type alias Model = {
  graphicsConfig: GraphicsConfig,
  mouseState: MouseState,
  cameraShift: Vec2.Vec2,
  graph: Graph.Graph Vertex (),
  intersections: List Intersection
  }

type alias GraphicsConfig = {
  vertexRadius: Float,
  vertexColor: Engine.Color,
  edgeWidth: Float,
  intersectionRadius: Float,
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

augmentEdgesWithEndsPositions : Graph.Graph Vertex () -> List (Graph.Edge ()) -> List (Graph.Edge (Vertex, Vertex))
augmentEdgesWithEndsPositions graph = List.map (\{from, to} ->
  let
    fromv = graph |> Graph.get from |> Maybe.map (\x -> x.node.label) |> Maybe.withDefault (0, 0) -- TODO: remove defaults
    tov = graph |> Graph.get to |> Maybe.map (\x -> x.node.label) |> Maybe.withDefault (0, 0) -- TODO: remove defaults
  in {
    from = from,
    to = to,
    label = (fromv, tov)
    })

initModel : Model
initModel =
  let
    graph = generateGraph (Random.initialSeed 12345) 20
    intersections = graph
      |> Graph.edges
      |> augmentEdgesWithEndsPositions graph
      |> squareList
      |> List.filter (\(e1, e2) -> (e1.from /= e2.from && e1.from /= e2.to && e1.to /= e2.from && e1.to /= e2.to))
      |> List.filter (\(e1, e2) -> ((e1.from, e1.to) < (e2.from, e2.to)))
      |> List.filterMap (\(e1, e2) ->
        let
          (from1v, to1v) = e1.label
          (from2v, to2v) = e2.label
          pt = intersectEdges (from1v, to1v) (from2v, to2v)
        in
          Maybe.map (\p -> {
            first = toEdge e1,
            second = toEdge e2,
            pt = p
            }) pt)
  in {
    graph = graph,
    mouseState = Up,
    intersections = intersections,
    graphicsConfig = initGraphicsConfig,
    cameraShift = (0, 0)
    }

generateGraph : Random.Seed -> Int -> Graph.Graph Vertex ()
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
      |> List.map Tuple.first
      |> List.map (\(from, to) -> Graph.Edge from to ())
      |> Graph.fromNodesAndEdges (vertices2 |> IntDict.values |> List.indexedMap Graph.Node)

isJust : Maybe a -> Bool
isJust x = case x of
  Just _ -> True
  Nothing -> False

squareList : List a -> List (a, a)
squareList xs = xs
  |> List.concatMap (\x -> xs |> List.map (Tuple.pair x))

main : Program () (Engine.Game Model) Engine.Msg
main = Engine.game render update initModel

applyTransforms : List (Engine.Transform -> Engine.Transform) -> Engine.Shape -> Engine.Shape
applyTransforms fs shape = {shape | transform = (List.foldl (\f g -> \x -> x |> g |> f) identity fs) shape.transform}

render : Engine.Computer -> Model -> List Engine.Shape
render computer model =
  let
    screen = computer.screen
    background = Engine.rectangle model.graphicsConfig.backgroundColor screen.width screen.height
    intersectionsText = model.intersections
      |> List.length
      |> String.fromInt
      |> Engine.words model.graphicsConfig.textColor
      |> applyTransforms [Engine.move (0, (screen.top - 20))]
    edges =
      let
        (heldEdges, notHeldEdges) = case model.mouseState of
          Vertex idx -> model.graph
            |> Graph.edges
            |> List.partition (\{from, to} -> from == idx || to == idx)
          _ -> ([], Graph.edges model.graph)
        colorEdges c ls = ls
          |> List.map (\{from, to} ->
            let
              fromv = model.graph |> Graph.get from |> Maybe.map (\x -> x.node.label) |> Maybe.withDefault (0, 0) -- TODO: remove defaults
              tov = model.graph |> Graph.get to |> Maybe.map (\x -> x.node.label) |> Maybe.withDefault (0, 0) -- TODO: remove defaults
            in
              (fromv, tov))
          |> List.map (\(x, y) -> Engine.path c (model.graphicsConfig.edgeWidth) [x, y])
        in
          (colorEdges model.graphicsConfig.notHeldEdgeColor notHeldEdges) ++
            (colorEdges model.graphicsConfig.heldEdgeColor heldEdges)
    vertices = model.graph
      |> Graph.nodes
      |> List.map .label
      |> List.map (\v -> Engine.circle model.graphicsConfig.vertexColor model.graphicsConfig.vertexRadius |> applyTransforms [Engine.move v])
    intersectionCircle = Engine.circle model.graphicsConfig.intersectionColor model.graphicsConfig.intersectionRadius
    intersections = model.intersections
      |> List.map .pt
      |> List.map (\v -> intersectionCircle |> applyTransforms [Engine.move v])
    transforms = (case model.mouseState of
      CameraMove cameraStart -> [Engine.move (Vec2.minus computer.mouse.pos cameraStart)]
      _ -> []) ++ [Engine.move model.cameraShift]
    graphRender = edges ++ vertices ++ intersections
  in
    background :: (graphRender |> List.map (applyTransforms transforms)) ++ [intersectionsText]

update : Engine.Computer -> Model -> Model
update computer model =
  let
    newCameraShift  = case (model.mouseState, computer.mouse.down) of
      (CameraMove cameraStart, False) -> Vec2.plus model.cameraShift (Vec2.minus computer.mouse.pos cameraStart)
      _ -> model.cameraShift
    -- TODO: 2 modes: down=take, up=release or click=take, click again=release
    newMouseState = case (model.mouseState, computer.mouse.down) of
      (Up, True) -> case chooseVertex
        (model.graph
          |> Graph.nodes
          |> List.map (\{id, label} -> (id, label))) model.graphicsConfig.vertexRadius (Vec2.minus computer.mouse.pos model.cameraShift) of
        Just idx -> Vertex idx
        Nothing -> CameraMove computer.mouse.pos
      (Vertex idx, True) -> Vertex idx
      (CameraMove cameraStart, True) -> CameraMove cameraStart
      (_, False) -> Up
    movedVertices = case newMouseState of
      Vertex i ->
        let
          newPos = Vec2.minus computer.mouse.pos model.cameraShift
          vs = model.graph
            |> Graph.nodes
            |> List.map (\n -> {n | label = if n.id == i then newPos else n.label})
          es = model.graph |> Graph.edges
        in
          Graph.fromNodesAndEdges vs es
      _ -> model.graph
    edges2 = movedVertices
      |> Graph.edges
      |> augmentEdgesWithEndsPositions movedVertices
    updatedIntersections = case newMouseState of
      Vertex idx -> Graph.get idx movedVertices
        |> Maybe.map (\ctx ->
          let
            tos = (Graph.alongOutgoingEdges ctx) ++ (Graph.alongIncomingEdges ctx)
          in
            tos |> List.map (\to -> Graph.Edge ctx.node.id to ())
        )
        |> Maybe.withDefault []
        |> augmentEdgesWithEndsPositions movedVertices
        |> List.concatMap (\e -> List.map (Tuple.pair e) edges2)
        |> List.filter (\(e1, e2) -> (e1.from /= e2.from && e1.from /= e2.to && e1.to /= e2.from && e1.to /= e2.to))
        |> List.filterMap (\(e1, e2) -> Maybe.map (\i -> {
          first = toEdge e1,
          second = toEdge e2,
          pt = i
          }) (intersectEdges e1.label e2.label))
      _ -> []
    newIntersections = case newMouseState of
      Vertex i -> model.intersections
        |> List.filter (\{first, second} -> (Tuple.first first) /= i && (Tuple.second first) /= i && (Tuple.first second) /= i && (Tuple.second second) /= i)
        |> (++) updatedIntersections
      _ -> model.intersections
    scale = 2 ^ (-computer.scroll / 1000)
    gg = Graph.mapNodes (Vec2.multiply scale) movedVertices
  in {model |
    graph = gg,
    mouseState = newMouseState,
    intersections = newIntersections,
    cameraShift = newCameraShift
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

-- TODO: take closest
chooseVertex : List (Graph.NodeId, Vertex) -> Float -> Vec2.Vec2 -> Maybe Graph.NodeId
chooseVertex vertices vertexRadius pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
