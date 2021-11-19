module Workspace exposing (..)

import Common exposing (Color, Dragging, Point, Radius, ShiftPressed, Thickness, Width, cycle, distanceToPoint, fromPoint, unique)
import Html exposing (Attribute)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Svg.Lazy



-- MODEL


type alias Model =
    { layers : List Layer
    , cursor : Point
    , focused : Bool
    , transform : Transform
    , canvas : Canvas
    , radius : Float
    , thickness : Float
    , tool : Tool
    , conductors : List ThroughConductor
    , nextNetId : Int -- Running id for nets
    , snapDistance : Float
    , autoNetColor : String
    , highlightNets : List Net
    , select : List VisualElement
    }


defaultModel : Model
defaultModel =
    { layers = []
    , cursor = Point 0 0
    , focused = False
    , transform = defaultTransform
    , canvas = { width = 0, height = 0 }
    , radius = 10
    , thickness = 5
    , tool = SelectTool Nothing
    , conductors = []
    , nextNetId = 1
    , snapDistance = 10
    , autoNetColor = ""
    , highlightNets = []
    , select = []
    }


type alias Layer =
    { id : Int
    , opacity : Int
    , conductors : List SurfaceConductor
    }


newLayer : Int -> Layer
newLayer layerId =
    { id = layerId
    , opacity = 100
    , conductors = []
    }


activeLayerSurfaceConductors : Model -> List SurfaceConductor
activeLayerSurfaceConductors model =
    Maybe.withDefault [] (Maybe.map (\l -> l.conductors) (List.head model.layers))


type alias Transform =
    { x : Float
    , y : Float
    , z : Float
    }


defaultTransform : Transform
defaultTransform =
    { x = 0, y = 0, z = 1 }


{-| Do panning translations to the transformation.

Panning should happen with the same speed as the mouse
moves on the screen, therefore the magnitude is tied to the
zoom multiplier.

-}
translateTransform : Transform -> Float -> Float -> Transform
translateTransform t x y =
    { t | x = t.x + x * t.z, y = t.y + y * t.z }


{-| Apply zoom to transform. While it accepts a float,
actually it only cares about the sign. This is due to
differences with browser wheel event delta's.
-}
zoomTransform : Transform -> Float -> Transform
zoomTransform t z =
    let
        -- 66.25 seems to be the default step for chrome on linux, so use that as a baseline
        multiplier =
            20 * (z / 66.25) / 100 + 1

        newZ =
            t.z * multiplier
    in
    { t | z = newZ }


type alias Canvas =
    { width : Float
    , height : Float
    }


type Net
    = NoNet Int
    | AutoNet Int
    | CustomNet String String


netsConductors : Model -> List Net -> List Conductor
netsConductors model nets =
    List.filter (\c -> List.member (conductorNet c) nets) (allConductors model)


type alias Pad =
    { number : Maybe Int
    , label : Maybe PadLabel
    }


type alias PadLabel =
    { text : String
    , rotation : Float
    }


type Conductor
    = Surface SurfaceConductor
    | Through ThroughConductor


allConductors : Model -> List Conductor
allConductors model =
    List.map Through model.conductors ++ List.concatMap (\layer -> List.map Surface layer.conductors) model.layers


conductorNet : Conductor -> Net
conductorNet conductor =
    case conductor of
        Through tht ->
            throughConductorNet tht

        Surface smt ->
            surfaceConductorNet smt


{-| Find out the shortest distance from a given point to a conductor
-}
distanceToConductor : Point -> Conductor -> Maybe ( Float, Point, Conductor )
distanceToConductor point conductor =
    conductorPoints conductor
        |> List.map (\conductorPoint -> ( distanceToPoint point conductorPoint, conductorPoint, conductor ))
        |> List.sortBy (\( distance, _, _ ) -> distance)
        |> List.head


conductorPoints : Conductor -> List Point
conductorPoints conductor =
    case conductor of
        Through tht ->
            throughConductorPoints tht

        Surface smt ->
            surfaceConductorPoints smt


type ThroughConductor
    = ThroughPad Pad Point Radius Net


throughConductorNet : ThroughConductor -> Net
throughConductorNet tht =
    case tht of
        ThroughPad _ _ _ net ->
            net


throughConductorPoints : ThroughConductor -> List Point
throughConductorPoints tht =
    case tht of
        ThroughPad _ point _ _ ->
            [ point ]


type SurfaceConductor
    = Trace (List TracePoint) Net
    | SurfacePad Pad Point Width Net
    | Zone (List Point) Net


type alias ColorDefinition =
    { stroke : String
    , fill : String
    }


type VisualElement
    = Circle Conductor Point Radius
    | ConstructionCircle Point Radius
    | Square Conductor Point Width (Maybe String)
    | SquareOutline Conductor Point Width (Maybe String)
    | ConstructionSquare Point Width (Maybe String)
    | Line Conductor Point Point Thickness
    | DashedLine Conductor Point Point Thickness
    | ConstructionLine Point Point Thickness


elementConductor : VisualElement -> Maybe Conductor
elementConductor element =
    case element of
        Circle conductor _ _ ->
            Just conductor

        ConstructionCircle _ _ ->
            Nothing

        Square conductor _ _ _ ->
            Just conductor

        SquareOutline conductor _ _ _ ->
            Just conductor

        ConstructionSquare _ _ _ ->
            Nothing

        Line conductor _ _ _ ->
            Just conductor

        DashedLine conductor _ _ _ ->
            Just conductor

        ConstructionLine _ _ _ ->
            Nothing


viewVisualElement : AppearanceDerivation a -> VisualElement -> Svg Msg
viewVisualElement model element =
    case element of
        Circle _ point radius ->
            viewCircle point
                radius
                [ SvgA.fill (deriveColor model element)
                , SvgE.onClick (ClickVisualElement element)
                , SvgA.class "clickable"
                ]

        ConstructionCircle point radius ->
            viewCircle point
                radius
                [ SvgA.stroke (deriveColor model element)
                , SvgA.fill "none"
                ]

        Square _ point width maybeText ->
            viewSquare point
                width
                [ SvgA.fill <| deriveColor model element
                ]
                Nothing

        SquareOutline _ point width maybeText ->
            viewSquare point
                width
                [ SvgA.stroke <| deriveColor model element
                , SvgA.strokeWidth "2px"
                , SvgA.fill "none"
                ]
                Nothing

        ConstructionSquare point width maybeText ->
            let
                color =
                    deriveColor model element
            in
            viewSquare point
                width
                [ SvgA.stroke <| color
                , SvgA.strokeWidth "2px"
                , SvgA.fill "none"
                ]
                (Maybe.map (\t -> ( t, color )) maybeText)

        Line _ p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                , SvgE.onClick (ClickVisualElement element)
                , SvgA.strokeLinecap "round"
                , SvgA.class "clickable"
                ]

        DashedLine _ p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                , SvgE.onClick (ClickVisualElement element)
                , SvgA.class "clickable"
                , SvgA.strokeDasharray <| String.join "," [ String.fromFloat (2 * thickness), String.fromFloat (1 * thickness) ]
                ]

        ConstructionLine p1 p2 thickness ->
            viewLine p1
                p2
                thickness
                [ SvgA.stroke <| deriveColor model element
                ]


viewCircle : Point -> Radius -> List (Svg.Attribute Msg) -> Svg Msg
viewCircle point radius attrs =
    Svg.circle
        ([ SvgA.cx <| String.fromFloat point.x
         , SvgA.cy <| String.fromFloat point.y
         , SvgA.r <| String.fromFloat radius
         ]
            ++ attrs
        )
        []


viewSquare : Point -> Width -> List (Svg.Attribute Msg) -> Maybe ( String, String ) -> Svg Msg
viewSquare point width attrs maybeText =
    let
        half =
            width / 2

        textElement =
            case maybeText of
                Just ( text, color ) ->
                    [ Svg.text_
                        [ SvgA.x <| String.fromFloat point.x
                        , SvgA.y <| String.fromFloat point.y
                        , SvgA.dominantBaseline "middle"
                        , SvgA.textAnchor "middle"
                        , SvgA.pointerEvents "none"
                        , SvgA.fontSize <| String.fromFloat width ++ "px"
                        , SvgA.fill color
                        ]
                        [ Svg.text text ]
                    ]

                Nothing ->
                    []
    in
    Svg.g [] <|
        [ Svg.rect
            ([ SvgA.x <| String.fromFloat (point.x - half)
             , SvgA.y <| String.fromFloat (point.y - half)
             , SvgA.width <| String.fromFloat width
             , SvgA.height <| String.fromFloat width
             ]
                ++ attrs
            )
            []
        ]
            ++ textElement


viewLine p1 p2 thickness attrs =
    let
        d =
            SvgA.d <|
                String.join " " <|
                    [ fromPoint "M" p1
                    , fromPoint "L" p2
                    ]
    in
    Svg.path
        ([ d
         , SvgA.fill "none"
         , SvgA.strokeWidth (String.fromFloat thickness)
         ]
            ++ attrs
        )
        []


conductorToVisualElement : Conductor -> List VisualElement
conductorToVisualElement conductor =
    case conductor of
        Surface surfaceConductor ->
            surfaceConductorToVisualElement False surfaceConductor

        Through throughConductor ->
            throughConductorToVisualElement throughConductor


throughConductorToVisualElement : ThroughConductor -> List VisualElement
throughConductorToVisualElement throughConductor =
    case throughConductor of
        ThroughPad pad point radius _ ->
            -- todo pad
            [ Circle (Through throughConductor) point radius ]


viewLazyThroughConductors : AppearanceDerivation a -> List ThroughConductor -> Svg Msg
viewLazyThroughConductors appearance throughConductors =
    Svg.Lazy.lazy3
        (\highlightNets select tc ->
            viewThroughConductors { highlightNets = highlightNets, select = select } tc
        )
        appearance.highlightNets
        appearance.select
        throughConductors


viewThroughConductors : AppearanceDerivation a -> List ThroughConductor -> Svg Msg
viewThroughConductors appearance throughConductors =
    Svg.g []
        (List.concatMap throughConductorToVisualElement throughConductors
            |> List.map (viewVisualElement appearance)
        )


surfaceConductorToVisualElement : Bool -> SurfaceConductor -> List VisualElement
surfaceConductorToVisualElement hidden surfaceConductor =
    let
        c =
            Surface surfaceConductor
    in
    case ( hidden, surfaceConductor ) of
        ( _, Trace tracePoints _ ) ->
            traceToVisualElements hidden c tracePoints

        -- todo
        ( False, SurfacePad pad point width _ ) ->
            -- todo pad
            [ Square c point width (Maybe.map String.fromInt pad.number) ]

        ( True, SurfacePad pad point width _ ) ->
            -- todo pad
            [ SquareOutline c point width (Maybe.map String.fromInt pad.number) ]

        ( _, Zone points net ) ->
            -- TODO
            []


traceToVisualElements : Bool -> Conductor -> List TracePoint -> List VisualElement
traceToVisualElements hidden c tracePoints =
    let
        element =
            if hidden then
                DashedLine

            else
                Line
    in
    case tracePoints of
        p1 :: p2 :: rest ->
            element c p1.point p2.point p2.thickness :: traceToVisualElements hidden c (p2 :: rest)

        _ ->
            []


surfaceConductorNet : SurfaceConductor -> Net
surfaceConductorNet smt =
    case smt of
        Trace _ net ->
            net

        SurfacePad _ _ _ net ->
            net

        Zone _ net ->
            net


surfaceConductorPoints : SurfaceConductor -> List Point
surfaceConductorPoints smt =
    case smt of
        Trace tracePoints _ ->
            List.map .point tracePoints

        SurfacePad _ point _ _ ->
            [ point ]

        Zone points _ ->
            points


type alias TracePoint =
    { point : Point
    , thickness : Float
    }


pointToTracePoint : Point -> Float -> TracePoint
pointToTracePoint point thickness =
    { point = point, thickness = thickness }


type ConstructionPoint a
    = FreePoint Point a
    | SnapPoint Point Conductor a


constructionPointPoint : ConstructionPoint a -> Point
constructionPointPoint sp =
    case sp of
        FreePoint p _ ->
            p

        SnapPoint p _ _ ->
            p


constructionPointConductor : ConstructionPoint a -> Maybe Conductor
constructionPointConductor sp =
    case sp of
        FreePoint p _ ->
            Nothing

        SnapPoint _ c _ ->
            Just c


constructionPointA : ConstructionPoint a -> a
constructionPointA cp =
    case cp of
        FreePoint _ a ->
            a

        SnapPoint _ _ a ->
            a


constructionPointToMaybeConductor : ConstructionPoint a -> Maybe Conductor
constructionPointToMaybeConductor cp =
    case cp of
        FreePoint _ _ ->
            Nothing

        SnapPoint _ conductor _ ->
            Just conductor


constructionPointsToConductors : List (ConstructionPoint a) -> List Conductor
constructionPointsToConductors cps =
    List.filterMap constructionPointToMaybeConductor cps


constructionPointToTracePoint : ConstructionPoint Thickness -> TracePoint
constructionPointToTracePoint cp =
    { point = constructionPointPoint cp, thickness = constructionPointA cp }


constructionPointsToTracePoints : List (ConstructionPoint Thickness) -> List TracePoint
constructionPointsToTracePoints cps =
    List.map constructionPointToTracePoint cps


constructionPointsToTrace : List (ConstructionPoint Thickness) -> Net -> SurfaceConductor
constructionPointsToTrace points net =
    -- todo net
    Trace (constructionPointsToTracePoints points) net


type Tool
    = SelectTool (Maybe Conductor)
    | CreateTraceTool (List (ConstructionPoint Thickness))
    | CreateSurfacePadTool
    | CreateNumberedSurfacePad Int
    | CreateSoicSurfacePad (Maybe Point) (Maybe Point)
    | CreateRowSurfacePad (Maybe Point) (Maybe Point)
    | CreateThroughPadTool
    | CreateNumberedThroughPad Int
    | CreateDipThroughPad (Maybe Point) (Maybe Point)
    | CreateRowThroughPad (Maybe Point) (Maybe Point)
    | CreateZoneTool


type MergeNet
    = MergeOk Net (List Conductor)
    | MergeConflict (List Net) (List Conductor)
    | MergeNoNet (List Conductor)


isCustomNet : Net -> Bool
isCustomNet net =
    case net of
        CustomNet _ _ ->
            True

        _ ->
            False


isAutoNet : Net -> Bool
isAutoNet net =
    case net of
        AutoNet _ ->
            True

        _ ->
            False


isNoNet : Net -> Bool
isNoNet net =
    case net of
        NoNet _ ->
            True

        _ ->
            False


mergeNets : Model -> List Conductor -> MergeNet
mergeNets model snappedConductors =
    let
        nets =
            List.map conductorNet snappedConductors
                |> unique

        conductors =
            netsConductors model nets

        ( customNets, autoNets ) =
            List.partition isCustomNet (List.filter (not << isNoNet) nets)
    in
    case ( customNets, autoNets ) of
        ( [ customNet ], _ ) ->
            MergeOk customNet conductors

        ( [], autoNet :: _ ) ->
            MergeOk autoNet conductors

        ( _ :: _, _ ) ->
            MergeConflict customNets conductors

        ( [], [] ) ->
            MergeNoNet conductors


type alias AppearanceDerivation a =
    { a | highlightNets : List Net, select : List VisualElement }


deriveColor : AppearanceDerivation a -> VisualElement -> String
deriveColor model element =
    let
        net =
            elementConductor element
                |> Maybe.map conductorNet
                |> Maybe.withDefault (NoNet 0)
    in
    if List.member element model.select then
        case net of
            NoNet _ ->
                "cyan"

            AutoNet _ ->
                "cyan"

            CustomNet _ c ->
                "cyan"

    else if List.member net model.highlightNets then
        case net of
            NoNet _ ->
                "cyan"

            AutoNet _ ->
                "cyan"

            CustomNet _ c ->
                "cyan"
        -- todo?

    else
        case net of
            NoNet _ ->
                "red"

            AutoNet _ ->
                "grey"

            CustomNet _ c ->
                c



-- UPDATE


type Msg
    = SetCursor Point ( Float, Float ) Dragging
    | LeftClick Point
    | ZoomDelta Float ShiftPressed
    | SetTool Tool
    | SetSubTool Int
    | ResetTool
    | SetTransform Transform
    | AddLayer Int
    | CycleLayers
    | Focus
    | Unfocus
    | ClickVisualElement VisualElement


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    case msg of
        SetCursor point ( dx, dy ) dragging ->
            if dragging then
                ( { model | cursor = point, transform = translateTransform model.transform dx dy }, Cmd.none, False )

            else
                case model.tool of
                    CreateTraceTool cps ->
                        let
                            snapPoint =
                                snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model) 0
                        in
                        ( { model | cursor = point }
                            |> createTraceToHighlightNets (snapPoint :: cps)
                        , Cmd.none
                        , False
                        )

                    _ ->
                        ( { model | cursor = point }, Cmd.none, False )

        LeftClick point ->
            let
                snapPoint =
                    snapTo model.snapDistance point model.conductors (activeLayerSurfaceConductors model)
            in
            if List.isEmpty model.layers then
                ( model, Cmd.none, False )

            else
                case model.tool of
                    CreateTraceTool points ->
                        case snapPoint model.thickness of
                            SnapPoint p c t ->
                                let
                                    newPoints =
                                        points ++ [ SnapPoint p c t ]
                                in
                                if List.isEmpty points then
                                    ( { model | tool = CreateTraceTool newPoints }
                                        |> createTraceToHighlightNets newPoints
                                    , Cmd.none
                                    , True
                                    )

                                else
                                    let
                                        mergedNet =
                                            mergeNets model (constructionPointsToConductors newPoints)
                                    in
                                    case mergedNet of
                                        MergeOk net conductors ->
                                            ( List.foldl (updateConductorNet net) model conductors
                                                |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                                |> resetModelTool
                                                |> createTraceToHighlightNets newPoints
                                            , Cmd.none
                                            , True
                                            )

                                        MergeConflict nets conductors ->
                                            -- TODO show somekind of conflict resolution thing
                                            ( resetModelTool model, Cmd.none, True )

                                        MergeNoNet conductors ->
                                            let
                                                net =
                                                    AutoNet model.nextNetId
                                            in
                                            ( List.foldl (updateConductorNet net) model conductors
                                                |> addSurfaceConductor (constructionPointsToTrace newPoints net)
                                                |> resetModelTool
                                                |> incrementNextNetId
                                                |> createTraceToHighlightNets newPoints
                                            , Cmd.none
                                            , True
                                            )

                            FreePoint p t ->
                                let
                                    t2 =
                                        points ++ [ FreePoint p t ]
                                in
                                if List.isEmpty points then
                                    ( model, Cmd.none, False )

                                else
                                    ( { model | tool = CreateTraceTool t2 }, Cmd.none, False )

                    CreateThroughPadTool ->
                        ( addThroughConductor (ThroughPad { number = Nothing, label = Nothing } point model.radius) model, Cmd.none, True )

                    CreateSurfacePadTool ->
                        ( addSurfaceConductorNoNet (SurfacePad { number = Nothing, label = Nothing } point (model.radius * 2)) model, Cmd.none, True )

                    CreateNumberedSurfacePad pinNumber ->
                        ( { model | tool = CreateNumberedSurfacePad (pinNumber + 1) }
                            |> addSurfaceConductorNoNet (SurfacePad { number = Just pinNumber, label = Nothing } point (model.radius * 2))
                        , Cmd.none
                        , True
                        )

                    CreateSoicSurfacePad mp1 mp2 ->
                        case ( mp1, mp2 ) of
                            ( Just _, Nothing ) ->
                                ( { model | tool = CreateSoicSurfacePad mp1 (Just point) }, Cmd.none, False )

                            ( Just p1, Just p2 ) ->
                                let
                                    shape =
                                        generateDoubleRow p1 p2 model.cursor
                                in
                                ( List.foldl
                                    (\( shapePoint, shapePad ) m ->
                                        addSurfaceConductorNoNet (SurfacePad shapePad shapePoint (model.radius * 2)) m
                                    )
                                    { model | tool = CreateSoicSurfacePad Nothing Nothing }
                                    shape
                                , Cmd.none
                                , True
                                )

                            _ ->
                                ( { model | tool = CreateSoicSurfacePad (Just point) Nothing }, Cmd.none, False )

                    CreateRowSurfacePad mp1 mp2 ->
                        case ( mp1, mp2 ) of
                            ( Just _, Nothing ) ->
                                ( { model | tool = CreateRowSurfacePad mp1 (Just point) }, Cmd.none, False )

                            ( Just p1, Just p2 ) ->
                                let
                                    shape =
                                        generateSingleRow p1 p2 model.cursor
                                in
                                ( List.foldl
                                    (\( shapePoint, shapePad ) m ->
                                        addSurfaceConductorNoNet (SurfacePad shapePad shapePoint (model.radius * 2)) m
                                    )
                                    { model | tool = CreateRowSurfacePad Nothing Nothing }
                                    shape
                                , Cmd.none
                                , True
                                )

                            _ ->
                                ( { model | tool = CreateRowSurfacePad (Just point) Nothing }, Cmd.none, False )

                    _ ->
                        ( model, Cmd.none, False )

        ZoomDelta delta shiftPressed ->
            if shiftPressed then
                case model.tool of
                    CreateThroughPadTool ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateSurfacePadTool ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateNumberedSurfacePad _ ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateSoicSurfacePad _ _ ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateRowSurfacePad _ _ ->
                        ( updateRadius delta model, Cmd.none, False )

                    CreateTraceTool _ ->
                        ( updateThickness delta model, Cmd.none, False )

                    _ ->
                        ( model, Cmd.none, False )

            else
                ( { model | transform = zoomTransform model.transform delta }, Cmd.none, False )

        SetTool tool ->
            case ( tool, model.tool ) of
                ( CreateTraceTool [], CreateTraceTool trace ) ->
                    ( { model
                        | tool =
                            CreateTraceTool
                                (trace
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                )
                      }
                    , Cmd.none
                    , False
                    )

                _ ->
                    ( { model | tool = tool, highlightNets = [] }, Cmd.none, False )

        ResetTool ->
            ( resetModelTool model, Cmd.none, False )

        SetTransform transform ->
            ( { model | transform = transform }, Cmd.none, False )

        CycleLayers ->
            ( { model | layers = cycle model.layers }, Cmd.none, False )

        AddLayer layerId ->
            ( { model | layers = newLayer layerId :: model.layers }, Cmd.none, True )

        Focus ->
            ( { model | focused = True }, Cmd.none, False )

        Unfocus ->
            ( { model | focused = False }, Cmd.none, False )

        ClickVisualElement element ->
            case element of
                Circle conductor point radius ->
                    ( model, Cmd.none, True )

                -- TODO
                ConstructionCircle point radius ->
                    ( model, Cmd.none, True )

                Square conductor point width _ ->
                    ( model, Cmd.none, True )

                SquareOutline conductor point width _ ->
                    ( model, Cmd.none, True )

                ConstructionSquare _ _ _ ->
                    ( model, Cmd.none, False )

                Line conductor p1 p2 _ ->
                    ( model, Cmd.none, True )

                DashedLine conductor p1 p2 _ ->
                    ( model, Cmd.none, True )

                ConstructionLine _ _ _ ->
                    ( model, Cmd.none, False )

        SetSubTool index ->
            case model.tool of
                SelectTool _ ->
                    ( model, Cmd.none, False )

                CreateTraceTool _ ->
                    ( model, Cmd.none, False )

                CreateSurfacePadTool ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateNumberedSurfacePad _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateSoicSurfacePad _ _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateRowSurfacePad _ _ ->
                    ( { model | tool = indexToSurfacePadTool index }, Cmd.none, False )

                CreateThroughPadTool ->
                    ( model, Cmd.none, False )

                CreateNumberedThroughPad _ ->
                    ( model, Cmd.none, False )

                CreateDipThroughPad _ _ ->
                    ( model, Cmd.none, False )

                CreateRowThroughPad _ _ ->
                    ( model, Cmd.none, False )

                CreateZoneTool ->
                    ( model, Cmd.none, False )


indexToSurfacePadTool : Int -> Tool
indexToSurfacePadTool index =
    case index of
        2 ->
            resetTool (CreateNumberedSurfacePad 1)

        3 ->
            resetTool (CreateSoicSurfacePad Nothing Nothing)

        4 ->
            resetTool (CreateRowSurfacePad Nothing Nothing)

        _ ->
            resetTool CreateSurfacePadTool



--ConductorPiece _ ->
--    ( { model | selectPieces = [ element ] }, Cmd.none, False )
--
--TraceSegmentPiece surfaceConductor p1 p2 ->
--    let
--        cs =
--            ConductorPiece (Surface surfaceConductor)
--    in
--    if List.member cs model.selectPieces then
--        ( { model
--            | selectPieces =
--                List.filter (\x -> x == cs) model.selectPieces
--                    |> List.append [ element ]
--          }
--        , Cmd.none
--        , True
--        )
--
--    else
--        ( { model | selectPieces = [ element ] }, Cmd.none, False )


createTraceToHighlightNets : List (ConstructionPoint Thickness) -> Model -> Model
createTraceToHighlightNets points model =
    { model | highlightNets = List.map conductorNet (constructionPointsToConductors points) }


updateRadius : Float -> Model -> Model
updateRadius delta model =
    let
        multiplier =
            1.25

        newZ =
            if delta < 0 then
                model.radius * multiplier

            else
                model.radius / multiplier
    in
    if newZ > 0.9 && newZ < 1.1 then
        { model | radius = 1 }

    else
        { model | radius = newZ }


updateThickness : Float -> Model -> Model
updateThickness delta model =
    let
        multiplier =
            1.25

        newZ =
            if delta < 0 then
                model.thickness * multiplier

            else
                model.thickness / multiplier
    in
    if newZ > 0.9 && newZ < 1.1 then
        { model | thickness = 1 }

    else
        { model | thickness = newZ }


resetTool : Tool -> Tool
resetTool tool =
    case tool of
        SelectTool _ ->
            SelectTool Nothing

        CreateTraceTool _ ->
            CreateTraceTool []

        CreateSurfacePadTool ->
            CreateSurfacePadTool

        CreateThroughPadTool ->
            CreateThroughPadTool

        CreateZoneTool ->
            CreateZoneTool

        CreateNumberedSurfacePad _ ->
            CreateNumberedSurfacePad 1

        CreateSoicSurfacePad _ _ ->
            CreateSoicSurfacePad Nothing Nothing

        CreateRowSurfacePad _ _ ->
            CreateRowSurfacePad Nothing Nothing

        CreateNumberedThroughPad _ ->
            CreateNumberedThroughPad 1

        CreateDipThroughPad _ _ ->
            CreateDipThroughPad Nothing Nothing

        CreateRowThroughPad _ _ ->
            CreateRowThroughPad Nothing Nothing


resetModelTool : Model -> Model
resetModelTool model =
    { model | tool = resetTool model.tool }


incrementNextNetId : Model -> Model
incrementNextNetId model =
    { model | nextNetId = model.nextNetId + 1 }


addThroughConductor : (Net -> ThroughConductor) -> Model -> Model
addThroughConductor toConductor model =
    case toConductor (NoNet model.nextNetId) of
        ThroughPad pad point radius net ->
            { model | nextNetId = model.nextNetId + 1, conductors = ThroughPad pad point radius net :: model.conductors }


addSurfaceConductorNoNet : (Net -> SurfaceConductor) -> Model -> Model
addSurfaceConductorNoNet toSurfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedLayer =
                    { layer | conductors = toSurfaceConductor (NoNet model.nextNetId) :: layer.conductors }
            in
            { model | layers = updatedLayer :: others, nextNetId = model.nextNetId + 1 }

        _ ->
            model


addSurfaceConductor : SurfaceConductor -> Model -> Model
addSurfaceConductor surfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedLayer =
                    { layer | conductors = surfaceConductor :: layer.conductors }
            in
            { model | layers = updatedLayer :: others }

        _ ->
            model


updateConductorNet : Net -> Conductor -> Model -> Model
updateConductorNet net conductor model =
    case conductor of
        Surface surfaceConductor ->
            updateSurfaceConductorNet net surfaceConductor model

        Through throughConductor ->
            let
                updatedThroughConductor =
                    case throughConductor of
                        ThroughPad pad point radius _ ->
                            ThroughPad pad point radius net
            in
            { model
                | conductors =
                    List.map
                        (\c ->
                            if c == throughConductor then
                                updatedThroughConductor

                            else
                                c
                        )
                        model.conductors
            }


updateSurfaceConductorNet : Net -> SurfaceConductor -> Model -> Model
updateSurfaceConductorNet net surfaceConductor model =
    case model.layers of
        layer :: others ->
            let
                updatedSurfaceConductor =
                    case surfaceConductor of
                        Trace tracePoints _ ->
                            Trace tracePoints net

                        SurfacePad pad point width _ ->
                            SurfacePad pad point width net

                        Zone points _ ->
                            Zone points net

                updatedConductors =
                    List.map
                        (\c ->
                            if c == surfaceConductor then
                                updatedSurfaceConductor

                            else
                                c
                        )
                        layer.conductors

                updatedLayer =
                    { layer | conductors = updatedConductors }
            in
            { model | layers = updatedLayer :: others }

        _ ->
            model



-- VIEWS


{-| Display anything and everything related to the active tool
-}
viewTool : Model -> Svg Msg
viewTool model =
    case model.tool of
        CreateTraceTool cps ->
            viewTraceWithConstruction model (constructionPointsToTracePoints cps)

        CreateThroughPadTool ->
            viewVisualElement model (ConstructionCircle model.cursor model.radius)

        CreateSurfacePadTool ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) Nothing)

        CreateNumberedSurfacePad pinNumber ->
            viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just <| String.fromInt pinNumber))

        CreateSoicSurfacePad mp1 mp2 ->
            case ( mp1, mp2 ) of
                -- todo simplify
                ( Just p1, Nothing ) ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionSquare p1 (model.radius * 2) (Just "1"))
                        , viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "2"))
                        ]

                ( Just p1, Just p2 ) ->
                    let
                        shape =
                            generateDoubleRow p1 p2 model.cursor
                    in
                    Svg.g [] <|
                        List.map (\( point, pad ) -> viewVisualElement model (ConstructionSquare point (model.radius * 2) (Maybe.map String.fromInt pad.number))) shape

                _ ->
                    viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "1"))

        CreateRowSurfacePad mp1 mp2 ->
            case ( mp1, mp2 ) of
                ( Just p1, Nothing ) ->
                    Svg.g []
                        [ viewVisualElement model (ConstructionSquare p1 (model.radius * 2) (Just "1"))
                        , viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "2"))
                        ]

                ( Just p1, Just p2 ) ->
                    let
                        shape =
                            generateSingleRow p1 p2 model.cursor
                    in
                    Svg.g [] <|
                        List.map (\( point, pad ) -> viewVisualElement model (ConstructionSquare point (model.radius * 2) (Maybe.map String.fromInt pad.number))) shape

                _ ->
                    viewVisualElement model (ConstructionSquare model.cursor (model.radius * 2) (Just "1"))

        _ ->
            Svg.text ""


vectorSum : Point -> Point -> Point
vectorSum p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


vectorMultiply : Point -> Float -> Point
vectorMultiply p m =
    { x = p.x * m, y = p.y * m }


dot : Point -> Point -> Float
dot p1 p2 =
    p1.x * p2.x + p1.y * p2.y


vectorLength : Point -> Float
vectorLength p =
    sqrt (p.x ^ 2 + p.y ^ 2)


vectorProjection : Point -> Point -> Point -> ( Point, Point, Point )
vectorProjection p1 p2 p3 =
    let
        -- Opposite vector a
        a : Point
        a =
            { x = p3.x - p1.x, y = p3.y - p1.y }

        -- Axis vector b
        b : Point
        b =
            { x = p2.x - p1.x, y = p2.y - p1.y }

        -- Axis length
        bl =
            vectorLength b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            dot a b / bl

        -- Projection
        p : Point
        p =
            { x = bu.x * pl, y = bu.y * pl }
    in
    ( a, b, p )


vectorRejection : Point -> Point -> Point
vectorRejection a p =
    { x = a.x - p.x, y = a.y - p.y }


generateProjectionRejection : Point -> Point -> Point -> ( Point, Point )
generateProjectionRejection p1 p2 p3 =
    let
        -- Opposite vector a
        a : Point
        a =
            { x = p3.x - p1.x, y = p3.y - p1.y }

        -- Axis vector b
        b : Point
        b =
            { x = p2.x - p1.x, y = p2.y - p1.y }

        -- Axis length
        bl =
            vectorLength b

        -- Axis unit
        bu : Point
        bu =
            { x = b.x / bl, y = b.y / bl }

        -- Projection length
        pl =
            dot a b / bl

        -- Projection
        p : Point
        p =
            { x = bu.x * pl, y = bu.y * pl }

        -- Axis normal
        bn =
            { x = -b.y, y = b.x }

        -- Axis normal unit
        bnu =
            { x = -bu.y, y = bu.x }

        -- Rejection
        r =
            { x = a.x - p.x, y = a.y - p.y }
    in
    ( p, r )


generateSingleRow : Point -> Point -> Point -> List ( Point, Pad )
generateSingleRow p1 p2 p3 =
    let
        ( a, b, p ) =
            vectorProjection p1 p2 p3

        count =
            round <| vectorLength p / vectorLength b
    in
    List.map (\number -> ( vectorSum p1 (vectorMultiply b (toFloat number)), { number = Just (number + 1), label = Nothing } )) (List.range 0 (max 1 count))


generateDoubleRow : Point -> Point -> Point -> List ( Point, Pad )
generateDoubleRow p1 p2 p3 =
    let
        ( a, b, p ) =
            vectorProjection p1 p2 p3

        r =
            vectorRejection a p

        p1r =
            vectorSum p1 r

        count =
            max 2 <| round <| vectorLength p / vectorLength b

        total =
            count * 2
    in
    List.map (\number -> ( vectorSum p1 (vectorMultiply b (toFloat number)), { number = Just (number + 1), label = Nothing } )) (List.range 0 count)
        ++ List.map
            (\number ->
                ( vectorSum p1r (vectorMultiply b (toFloat number))
                , { number = Just (total - number + 2), label = Nothing }
                )
            )
            (List.range 0 count)


toolToString : Tool -> String
toolToString tool =
    case tool of
        SelectTool _ ->
            "select"

        CreateTraceTool _ ->
            "trace"

        CreateSurfacePadTool ->
            "surface"

        CreateThroughPadTool ->
            "through"

        CreateZoneTool ->
            "zone"

        CreateNumberedSurfacePad _ ->
            "numbered-surface"

        CreateSoicSurfacePad _ _ ->
            "soic-surface"

        CreateRowSurfacePad _ _ ->
            "row-surface"

        CreateNumberedThroughPad _ ->
            "numbered-thorugh"

        CreateDipThroughPad _ _ ->
            "dip-through"

        CreateRowThroughPad _ _ ->
            "row-through"


viewSurfaceConductors : AppearanceDerivation a -> List Layer -> Svg Msg
viewSurfaceConductors model layers =
    case layers of
        layer :: hiddenLayers ->
            let
                highlightedHiddenSurfaceConductors =
                    List.concatMap .conductors hiddenLayers
                        |> List.filter (\c -> List.member (surfaceConductorNet c) model.highlightNets)

                f hidden =
                    List.map (viewVisualElement model) << surfaceConductorToVisualElement hidden
            in
            Svg.Lazy.lazy2
                (\c cHidden ->
                    Svg.g [] <|
                        List.concatMap (f False) c
                            ++ List.concatMap (f True) cHidden
                )
                layer.conductors
                highlightedHiddenSurfaceConductors

        [] ->
            Svg.text ""


viewTraceWithConstruction : Model -> List TracePoint -> Svg Msg
viewTraceWithConstruction model points =
    let
        constructionTrace =
            if model.focused then
                viewConstructionTrace model points

            else
                []
    in
    Svg.g [] <|
        viewTraceSegmented Nothing "lime" [] points
            ++ constructionTrace


viewCrosshair : ConstructionPoint a -> Svg Msg
viewCrosshair point =
    let
        length =
            35

        ( center, color ) =
            case point of
                SnapPoint p _ _ ->
                    ( p, "lime" )

                FreePoint p _ ->
                    ( p, "red" )
    in
    Svg.g []
        [ Svg.line
            [ SvgA.x1 <| String.fromFloat <| center.x - length
            , SvgA.x2 <| String.fromFloat <| center.x + length
            , SvgA.y1 <| String.fromFloat <| center.y
            , SvgA.y2 <| String.fromFloat <| center.y
            , SvgA.stroke color
            ]
            []
        , Svg.line
            [ SvgA.x1 <| String.fromFloat <| center.x
            , SvgA.x2 <| String.fromFloat <| center.x
            , SvgA.y1 <| String.fromFloat <| center.y - length
            , SvgA.y2 <| String.fromFloat <| center.y + length
            , SvgA.stroke color
            ]
            []
        ]


viewTraceSegment : Maybe (Point -> Point -> Msg) -> String -> List (Attribute Msg) -> TracePoint -> TracePoint -> Svg Msg
viewTraceSegment maybeToClickMsg color attrs start end =
    let
        d =
            SvgA.d <|
                String.join " " <|
                    [ fromPoint "M" start.point
                    , fromPoint "L" end.point
                    ]
    in
    Svg.path
        ([ d
         , SvgA.fill "none"
         , SvgA.stroke color
         , SvgA.strokeWidth (String.fromFloat end.thickness)
         , SvgA.strokeLinecap "round"
         ]
            ++ Maybe.withDefault [] (Maybe.map (\f -> [ SvgE.onClick (f start.point end.point), SvgA.class "clickable" ]) maybeToClickMsg)
            ++ attrs
        )
        []


viewTraceSegmented : Maybe (Point -> Point -> Msg) -> String -> List (Attribute Msg) -> List TracePoint -> List (Svg Msg)
viewTraceSegmented maybeToClickMsg color attrs tracePoints =
    case tracePoints of
        start :: end :: rest ->
            viewTraceSegment maybeToClickMsg color attrs start end :: viewTraceSegmented maybeToClickMsg color attrs (end :: rest)

        _ ->
            [ Svg.text "" ]



-- todo this one has confusing name with the other similar function, fix


viewConstructionTrace : Model -> List TracePoint -> List (Svg Msg)
viewConstructionTrace model tracePoints =
    let
        cp =
            snapTo model.snapDistance model.cursor model.conductors (activeLayerSurfaceConductors model) model.thickness

        point =
            case cp of
                SnapPoint p _ _ ->
                    p

                FreePoint p _ ->
                    p
    in
    case List.reverse tracePoints of
        last :: _ ->
            [ viewTraceSegment Nothing "lime" [] last (pointToTracePoint point model.thickness)
            , viewCrosshair cp
            ]

        _ ->
            [ viewCrosshair cp ]



-- UTILITY


snapTo : Float -> Point -> List ThroughConductor -> List SurfaceConductor -> (a -> ConstructionPoint a)
snapTo maxSnapDistance point thts smts =
    let
        closest =
            List.map Through thts
                ++ List.map Surface smts
                |> List.filterMap (distanceToConductor point)
                |> List.sortBy (\( d, _, _ ) -> d)
                |> List.head
    in
    case closest of
        Just ( conductorDistance, conductorPoint, conductor ) ->
            if snaps conductor conductorDistance maxSnapDistance then
                SnapPoint conductorPoint conductor

            else
                FreePoint point

        Nothing ->
            FreePoint point


snaps : Conductor -> Float -> Float -> Bool
snaps conductor conductorDistance defaultSnapDistance =
    case conductor of
        Surface _ ->
            conductorDistance < defaultSnapDistance

        Through tc ->
            case tc of
                ThroughPad _ _ radius _ ->
                    conductorDistance <= radius
