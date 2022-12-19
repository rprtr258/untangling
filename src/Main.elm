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
    transforms = (case model.mouseState of
      CameraMove cameraStart -> [Engine.move (Vec2.minus computer.mouse.pos cameraStart)]
      _ -> []) ++ [Engine.move model.cameraShift]
    graphRender = intersections
  in
      (graphRender |> List.map (applyTransforms transforms)) ++

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

-- TODO: take closest
chooseVertex : List (Graph.NodeId, Vertex) -> Float -> Vec2.Vec2 -> Maybe Graph.NodeId
chooseVertex vertices vertexRadius pos = vertices
  |> List.filter (\(_, v) -> ((Vec2.distSquared v pos) < vertexRadius ^ 2))
  |> List.head
  |> Maybe.map Tuple.first
